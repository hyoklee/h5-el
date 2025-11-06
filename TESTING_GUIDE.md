# Testing Guide for HDF5 Version 2 Support

## What Was Fixed

### Critical Bug: Superblock Reading

The previous implementation had **incorrect byte offsets** when reading superblocks, which prevented proper navigation of HDF5 files. This has been fixed for all superblock versions.

#### Superblock v2/v3 (Modern HDF5 >= 1.8)
**Before**: Reading from wrong positions (off by 3-4 bytes)
**After**: Correctly reads from file offsets 12, 20, 28, 36

#### Superblock v0/v1 (Legacy HDF5 < 1.8)
**Before**: Reading root group address from position 48 (incorrect)
**After**: Correctly reads from position 65 (file offset 64) within the root group symbol table entry

### Impact

✅ **Fixed**: Root group can now be located correctly
✅ **Fixed**: Datasets and groups under root group are now visible
✅ **Fixed**: Works with both modern (v2/v3) and legacy (v0/v1) HDF5 files

## Test Files Analysis

### 1. simple.h5 (Superblock v2)
- **Root group at**: Address 0x30 (48)
- **Object header**: Version 2 with OHDR signature
- **Contents**: No datasets/groups in root (only has attributes)
- **Expected result**: Shows root group, no children

### 2. with_groups.h5 (Superblock v2) ⭐
- **Root group at**: Address 0x30 (48)
- **Object header**: Version 2 with link messages
- **Contents**:
  - `dataset1` at address 0xc3 (195)
  - `subgroup` at address 0x1cf (463)
- **Expected result**: Shows root group with 2 children

### 3. with_attributes.h5 (Superblock v0)
- **Root group at**: Address 0x60 (96)
- **Object header**: Version 1
- **Expected result**: Shows root group and its contents

### 4. v0.h5 (Superblock v0)
- **Root group at**: To be determined
- **Object header**: Version 1
- **Expected result**: Shows root group and its contents

## How to Test in Emacs

### Quick Test (Interactive)

```elisp
;; Load the fixed code
(load-file "/path/to/h5-io.el")
(load-file "/path/to/h5-test.el")

;; Run all tests
(h5-test-run-all)
```

### Manual Test with with_groups.h5

```elisp
(let ((file (h5-io-open "/path/to/testdata/with_groups.h5")))
  (unwind-protect
      (progn
        (message "Testing with_groups.h5...")
        (message "Root group: %S" (h5-io-file-root-group file))

        ;; Walk all objects
        (h5-io-walk file
                   (lambda (path obj)
                     (cond
                      ((h5-io-group-p obj)
                       (message "Group: %s (children: %d)"
                               path
                               (length (h5-io-group-children obj))))
                      ((h5-io-dataset-p obj)
                       (message "Dataset: %s" path))))))
    (h5-io-close file)))
```

**Expected output:**
```
Group: / (children: 2)
Dataset: /dataset1
Group: /subgroup (children: X)
... more entries ...
```

### Test from h5-mode

1. Open dired in the testdata directory
2. Navigate to `with_groups.h5`
3. Press `C-c h` to open in h5-mode
4. You should see:
   - Root group "/"
   - Child "dataset1"
   - Child "subgroup"

### Detailed Debug Test

```elisp
(let ((file (h5-io-open "/path/to/testdata/with_groups.h5")))
  (unwind-protect
      (let* ((sb (h5-io-file-superblock file))
             (root-addr (h5-io-superblock-root-group-object-header-address sb))
             (root (h5-io-file-root-group file)))
        (message "Superblock version: %d" (h5-io-superblock-version sb))
        (message "Root group address: %d (0x%x)" root-addr root-addr)
        (message "Root group name: %s" (h5-io-group-name root))
        (message "Root group children: %S" (h5-io-group-children root))
        (message "Number of children: %d" (length (h5-io-group-children root)))

        ;; Print each child
        (dolist (child (h5-io-group-children root))
          (message "  Child: %s -> address %d (0x%x)"
                  (car child) (cdr child) (cdr child))))
    (h5-io-close file)))
```

**Expected output for with_groups.h5:**
```
Superblock version: 2
Root group address: 48 (0x30)
Root group name: /
Root group children: (("dataset1" . 195) ("subgroup" . 463))
Number of children: 2
  Child: dataset1 -> address 195 (0xc3)
  Child: subgroup -> address 463 (0x1cf)
```

## What to Look For

### ✅ Success Indicators
- Root group is found at the correct address
- Children list is populated (not empty for with_groups.h5)
- h5-io-walk prints all paths correctly
- No error messages about undefined addresses

### ❌ Failure Indicators
- Root group address is 0 or 0xFFFFFFFFFFFFFFFF
- Children list is empty when it shouldn't be
- Error: "Invalid OHDR signature"
- Error: "Invalid symbol table node signature"

## Commits

1. **3648c52**: feat: implement full HDF5 version 2 object header and link message support
2. **b14663f**: fix: correct superblock reading for v0/v1 and v2/v3

## Next Steps If Tests Pass

Once you confirm the tests pass:
1. Test with your own HDF5 files
2. Report any files that don't work correctly
3. Consider implementing dense link storage (for very large groups)
4. Consider implementing object header continuation blocks (for objects with many attributes)

## If Tests Fail

If you still don't see datasets, please provide:
1. The exact HDF5 file you're testing with
2. The output of the debug test above
3. Any error messages you see
4. Output of `h5dump -H yourfile.h5` (if available)

## Technical Details

### Byte Offset Corrections

**Superblock v0/v1:**
- Old: Root group at Emacs position 48 (file offset 47) ❌
- New: Root group at Emacs position 65 (file offset 64) ✅

**Superblock v2/v3:**
- Old: Root group at Emacs position 40 (file offset 39) ❌
- New: Root group at Emacs position 37 (file offset 36) ✅

The bug was caused by confusion between:
- **Emacs buffer positions** (1-indexed, starting at position 1)
- **File offsets** (0-indexed, starting at offset 0)

The relationship is: `Emacs position = File offset + 1`

All code now includes detailed comments showing both the Emacs position and the corresponding file offset for clarity.
