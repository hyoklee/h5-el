#!/usr/bin/env python3
"""Detailed HDF5 file inspection."""

import struct
import sys
import os

def read_uint8(f):
    return struct.unpack('<B', f.read(1))[0]

def read_uint16(f):
    return struct.unpack('<H', f.read(2))[0]

def read_uint32(f):
    return struct.unpack('<I', f.read(4))[0]

def read_uint64(f):
    return struct.unpack('<Q', f.read(8))[0]

def inspect_file_detailed(filename):
    """Detailed inspection of HDF5 file."""
    print(f"\n{'='*70}")
    print(f"File: {filename}")
    print(f"{'='*70}")

    with open(filename, 'rb') as f:
        # Check signature
        sig = f.read(8)
        if sig != b'\x89HDF\r\n\x1a\n':
            print("ERROR: Not a valid HDF5 file")
            return

        # Read superblock version
        f.seek(8)
        version = read_uint8(f)
        print(f"Superblock version: {version}")

        # Read based on version
        root_addr = None

        if version in [0, 1]:
            f.seek(13)  # Offset 13: size of offsets
            size_of_offsets = read_uint8(f)
            size_of_lengths = read_uint8(f)
            print(f"Size of offsets: {size_of_offsets}")
            print(f"Size of lengths: {size_of_lengths}")

            # For v0/v1: file offset 24 = base, 32 = free, 40 = eof, 48 = driver
            # Root group symbol table entry at offset 56, object header address at offset 64
            f.seek(24)
            base_addr = read_uint64(f)
            f.seek(40)
            eof_addr = read_uint64(f)
            f.seek(64)
            root_addr = read_uint64(f)

        elif version in [2, 3]:
            f.seek(9)  # Offset 9: size of offsets
            size_of_offsets = read_uint8(f)
            size_of_lengths = read_uint8(f)
            print(f"Size of offsets: {size_of_offsets}")
            print(f"Size of lengths: {size_of_lengths}")

            # For v2: file offset 12 = base, 20 = ext, 28 = eof, 36 = root
            f.seek(12)
            base_addr = read_uint64(f)
            f.seek(20)
            ext_addr = read_uint64(f)
            f.seek(28)
            eof_addr = read_uint64(f)
            f.seek(36)
            root_addr = read_uint64(f)

        print(f"Root group object header address: {root_addr:#x} ({root_addr})")

        if root_addr is None or root_addr == 0 or root_addr == 0xFFFFFFFFFFFFFFFF:
            print("ERROR: Invalid root group address!")
            return

        # Inspect root object header
        print(f"\nRoot Object Header:")
        f.seek(root_addr)

        # Check for OHDR signature
        sig_check = f.read(4)
        f.seek(root_addr)

        if sig_check == b'OHDR':
            print(f"  Format: Version 2 (OHDR signature found)")
            f.seek(root_addr + 4)
            oh_version = read_uint8(f)
            flags = read_uint8(f)

            print(f"  Version: {oh_version}")
            print(f"  Flags: {flags:#04x}")

            # Decode flags
            size_field_size = 1 << (flags & 0x03)
            track_attr = bool(flags & 0x04)
            index_attr = bool(flags & 0x08)
            phase_change = bool(flags & 0x10)
            store_times = bool(flags & 0x20)

            print(f"    Size of chunk size field: {size_field_size} bytes")
            print(f"    Track attribute order: {track_attr}")
            print(f"    Index attribute order: {index_attr}")
            print(f"    Store phase change: {phase_change}")
            print(f"    Store times: {store_times}")

            # Calculate message start position
            pos = root_addr + 6
            if store_times:
                pos += 16
            if phase_change:
                pos += 4

            # Read chunk size
            f.seek(pos)
            if size_field_size == 1:
                chunk_size = read_uint8(f)
            elif size_field_size == 2:
                chunk_size = read_uint16(f)
            elif size_field_size == 4:
                chunk_size = read_uint32(f)
            else:
                chunk_size = read_uint64(f)

            print(f"  Chunk 0 size: {chunk_size} bytes")

            # Read messages
            msg_start = pos + size_field_size
            msg_end = msg_start + chunk_size - 4  # -4 for checksum

            print(f"\n  Messages (start={msg_start:#x}, end={msg_end:#x}):")

            f.seek(msg_start)
            msg_count = 0
            while f.tell() < msg_end - 4:
                msg_type = read_uint8(f)
                if msg_type == 0:  # NIL/padding
                    break

                msg_size = read_uint16(f)
                msg_flags = read_uint8(f)

                msg_names = {
                    0x01: "Dataspace",
                    0x03: "Datatype",
                    0x06: "Link",
                    0x08: "Data Layout",
                    0x0A: "Group Info",
                    0x11: "Symbol Table",
                    0x15: "Attribute Info",
                    0x0C: "Attribute",
                }

                msg_name = msg_names.get(msg_type, f"Unknown-{msg_type:#04x}")
                print(f"    [{msg_count}] Type={msg_type:#04x} ({msg_name}), Size={msg_size}, Flags={msg_flags:#04x}")

                # If it's a link message, try to parse it
                if msg_type == 0x06:
                    link_start = f.tell()
                    link_version = read_uint8(f)
                    link_flags = read_uint8(f)

                    name_size_size = 1 << (link_flags & 0x03)
                    has_creation_order = bool(link_flags & 0x04)
                    has_link_type = bool(link_flags & 0x08)
                    has_charset = bool(link_flags & 0x10)

                    link_type = 0  # Default to hard link
                    if has_link_type:
                        link_type = read_uint8(f)

                    if has_creation_order:
                        f.seek(f.tell() + 8)
                    if has_charset:
                        f.seek(f.tell() + 1)

                    # Read name length
                    if name_size_size == 1:
                        name_len = read_uint8(f)
                    elif name_size_size == 2:
                        name_len = read_uint16(f)
                    elif name_size_size == 4:
                        name_len = read_uint32(f)
                    else:
                        name_len = read_uint64(f)

                    # Read name
                    name_bytes = f.read(name_len)
                    try:
                        name = name_bytes.decode('utf-8').rstrip('\x00')
                    except:
                        name = name_bytes.hex()

                    # Read link info
                    if link_type == 0:  # Hard link
                        obj_addr = read_uint64(f)
                        print(f"        -> LINK: '{name}' -> address {obj_addr:#x}")
                    else:
                        print(f"        -> LINK: '{name}' (type={link_type})")

                    f.seek(link_start + msg_size)
                else:
                    # Skip message data
                    f.seek(f.tell() + msg_size)

                msg_count += 1
                if msg_count > 50:
                    print("    ... (stopped after 50 messages)")
                    break

        else:
            # Version 1 object header
            print(f"  Format: Version 1")
            oh_version = read_uint8(f)
            f.seek(f.tell() + 1)  # Reserved
            num_msgs = read_uint16(f)
            ref_count = read_uint32(f)
            header_size = read_uint32(f)

            print(f"  Version: {oh_version}")
            print(f"  Number of messages: {num_msgs}")
            print(f"  Reference count: {ref_count}")
            print(f"  Header size: {header_size}")

            print(f"\n  Messages:")
            msg_pos = root_addr + 16
            for i in range(min(num_msgs, 50)):
                f.seek(msg_pos)
                msg_type = read_uint16(f)
                msg_size = read_uint16(f)
                msg_flags = read_uint8(f)
                f.seek(f.tell() + 3)  # Reserved

                msg_names = {
                    0x01: "Dataspace",
                    0x03: "Datatype",
                    0x06: "Link",
                    0x08: "Data Layout",
                    0x0A: "Group Info",
                    0x11: "Symbol Table",
                    0x15: "Attribute Info",
                    0x0C: "Attribute",
                }

                msg_name = msg_names.get(msg_type, f"Unknown")
                print(f"    [{i}] Type={msg_type:#06x} ({msg_name}), Size={msg_size}, Flags={msg_flags:#04x}")

                # If symbol table, parse it
                if msg_type == 0x11:
                    btree_addr = read_uint64(f)
                    heap_addr = read_uint64(f)
                    print(f"        -> SYMBOL TABLE: B-tree={btree_addr:#x}, Heap={heap_addr:#x}")

                msg_pos = msg_pos + 8 + msg_size
                # Align to 8 bytes
                msg_pos = ((msg_pos + 7) // 8) * 8

if __name__ == '__main__':
    files = ['simple.h5', 'with_groups.h5', 'with_attributes.h5', 'v0.h5']

    for filename in files:
        filepath = os.path.join('testdata', filename)
        if os.path.exists(filepath):
            inspect_file_detailed(filepath)
