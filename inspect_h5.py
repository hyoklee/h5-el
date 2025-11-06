#!/usr/bin/env python3
"""Inspect HDF5 files to understand their structure."""

import struct
import sys
import os

def read_bytes(f, n):
    """Read n bytes from file."""
    return f.read(n)

def read_uint8(f):
    """Read 1-byte unsigned integer."""
    return struct.unpack('<B', f.read(1))[0]

def read_uint16(f):
    """Read 2-byte unsigned integer (little-endian)."""
    return struct.unpack('<H', f.read(2))[0]

def read_uint32(f):
    """Read 4-byte unsigned integer (little-endian)."""
    return struct.unpack('<I', f.read(4))[0]

def read_uint64(f):
    """Read 8-byte unsigned integer (little-endian)."""
    return struct.unpack('<Q', f.read(8))[0]

def inspect_hdf5_file(filename):
    """Inspect an HDF5 file and print its structure."""
    print(f"\n{'='*60}")
    print(f"Inspecting: {filename}")
    print(f"{'='*60}")

    with open(filename, 'rb') as f:
        # Read superblock signature
        sig = f.read(8)
        if sig != b'\x89HDF\r\n\x1a\n':
            print("ERROR: Invalid HDF5 signature!")
            return

        print("✓ Valid HDF5 signature")

        # Read superblock version
        version = read_uint8(f)
        print(f"Superblock version: {version}")

        # Skip to root group address based on version
        if version == 0 or version == 1:
            f.seek(8 + 1 + 1 + 1 + 1 + 1 + 1 + 2)  # Skip to size fields
            size_of_offsets = read_uint8(f)
            size_of_lengths = read_uint8(f)
            f.seek(8 + 1 + 1 + 1 + 1 + 1 + 1 + 2 + 1 + 1 + 2 + 2)

            # Read root group symbol table entry address
            if size_of_offsets == 8:
                root_addr = read_uint64(f)
            elif size_of_offsets == 4:
                root_addr = read_uint32(f)
            else:
                root_addr = 0

            print(f"Size of offsets: {size_of_offsets}")
            print(f"Size of lengths: {size_of_lengths}")
            print(f"Root group address: {root_addr:#x} ({root_addr})")

            # Try to read object header at root address
            if root_addr > 0:
                f.seek(root_addr)

                # Check for OHDR signature (v2)
                sig_bytes = f.read(4)
                f.seek(root_addr)

                if sig_bytes == b'OHDR':
                    print(f"✓ Found OHDR signature - Object Header Version 2")
                    f.seek(root_addr + 4)
                    oh_version = read_uint8(f)
                    flags = read_uint8(f)
                    print(f"  Object header version: {oh_version}")
                    print(f"  Flags: {flags:#04x} (binary: {bin(flags)})")

                    # Parse flags
                    size_field_size = 1 << (flags & 0x03)
                    has_times = bool(flags & 0x20)
                    has_phase_change = bool(flags & 0x10)

                    print(f"  Size of 'size of chunk 0' field: {size_field_size} bytes")
                    print(f"  Store times: {has_times}")
                    print(f"  Store phase change: {has_phase_change}")

                    # Skip optional fields and read chunk size
                    f.seek(root_addr + 6)
                    if has_times:
                        f.seek(f.tell() + 16)
                    if has_phase_change:
                        f.seek(f.tell() + 4)

                    # Read chunk0 size
                    if size_field_size == 1:
                        chunk0_size = read_uint8(f)
                    elif size_field_size == 2:
                        chunk0_size = read_uint16(f)
                    elif size_field_size == 4:
                        chunk0_size = read_uint32(f)
                    else:
                        chunk0_size = read_uint64(f)

                    print(f"  Chunk 0 size: {chunk0_size} bytes")

                    # Read messages
                    print(f"\n  Messages in object header:")
                    msg_start = f.tell()
                    msg_count = 0
                    while f.tell() < root_addr + 6 + (16 if has_times else 0) + (4 if has_phase_change else 0) + size_field_size + chunk0_size - 4:
                        msg_type = read_uint8(f)
                        msg_size = read_uint16(f)
                        msg_flags = read_uint8(f)

                        if msg_type == 0:  # NIL message (padding)
                            break

                        msg_type_names = {
                            0x0001: "Dataspace",
                            0x0003: "Datatype",
                            0x0006: "Link",
                            0x0008: "Data Layout",
                            0x000A: "Group Info",
                            0x0011: "Symbol Table",
                            0x0015: "Attribute Info",
                        }

                        msg_name = msg_type_names.get(msg_type, f"Unknown ({msg_type:#04x})")
                        print(f"    [{msg_count}] Type: {msg_type:#04x} ({msg_name}), Size: {msg_size}, Flags: {msg_flags:#04x}")

                        # Skip message data
                        f.seek(f.tell() + msg_size)
                        msg_count += 1

                        if msg_count > 20:  # Safety limit
                            print("    ... (stopped after 20 messages)")
                            break

                else:
                    # Version 1 object header
                    version_byte = read_uint8(f)
                    f.seek(root_addr + 1)
                    print(f"Object Header Version 1 (version byte: {version_byte})")

                    # Skip reserved byte
                    f.seek(root_addr + 2)
                    num_messages = read_uint16(f)
                    obj_ref_count = read_uint32(f)
                    obj_header_size = read_uint32(f)

                    print(f"  Number of messages: {num_messages}")
                    print(f"  Reference count: {obj_ref_count}")
                    print(f"  Header size: {obj_header_size}")

                    # Read messages
                    print(f"\n  Messages in object header:")
                    f.seek(root_addr + 16)
                    for i in range(min(num_messages, 20)):
                        msg_type = read_uint16(f)
                        msg_size = read_uint16(f)
                        msg_flags = read_uint8(f)
                        f.seek(f.tell() + 3)  # Skip reserved bytes

                        msg_type_names = {
                            0x0001: "Dataspace",
                            0x0003: "Datatype",
                            0x0006: "Link",
                            0x0008: "Data Layout",
                            0x000A: "Group Info",
                            0x0011: "Symbol Table",
                            0x0015: "Attribute Info",
                        }

                        msg_name = msg_type_names.get(msg_type, f"Unknown")
                        print(f"    [{i}] Type: {msg_type:#06x} ({msg_name}), Size: {msg_size}, Flags: {msg_flags:#04x}")

                        # Skip message data and alignment
                        f.seek(f.tell() + msg_size)
                        # Align to 8 bytes
                        pos = f.tell()
                        aligned_pos = ((pos + 7) // 8) * 8
                        f.seek(aligned_pos)

if __name__ == '__main__':
    testdata_dir = 'testdata'
    files = ['simple.h5', 'with_groups.h5', 'with_attributes.h5', 'v0.h5']

    for filename in files:
        filepath = os.path.join(testdata_dir, filename)
        if os.path.exists(filepath):
            inspect_hdf5_file(filepath)
        else:
            print(f"File not found: {filepath}")
