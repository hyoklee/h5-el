# h5-el
Emacs HDF5 mode with pure Emacs Lisp HDF5 file I/O

## Overview

This package provides two main components:

1. **h5-mode.el** - An interactive major mode for viewing/editing HDF5 files (similar to Dired)
2. **h5-io.el** - Pure Emacs Lisp library for reading and writing HDF5 files

## h5-io.el - Pure Emacs Lisp HDF5 Library

A pure Emacs Lisp implementation for reading and writing HDF5 files, inspired by the [scigolib/hdf5](https://github.com/scigolib/hdf5) Go library.

### Features

- **Pure Emacs Lisp** - No external dependencies or C bindings required
- **Reading HDF5 files** - Parse superblock, groups, datasets, and attributes
- **Writing HDF5 files** - Create files, datasets, groups, and attributes
- **High-level API** - Similar to the Go scigolib/hdf5 library
- **Multiple data types** - Support for integers, floats, strings
- **Multi-dimensional datasets** - Support for 1D, 2D, and N-D arrays

### Quick Start

#### Reading HDF5 Files

```elisp
(require 'h5-io)

;; Open file
(let ((file (h5-io-open "data.h5")))
  (unwind-protect
      (progn
        ;; Get superblock info
        (let ((sb (h5-io-file-superblock file)))
          (message "Version: %d" (h5-io-superblock-version sb)))

        ;; Walk through all objects
        (h5-io-walk file
                   (lambda (path obj)
                     (cond
                      ((h5-io-group-p obj)
                       (message "Group: %s" path))
                      ((h5-io-dataset-p obj)
                       (message "Dataset: %s" path))))))

    ;; Always close the file
    (h5-io-close file)))
```

#### Writing HDF5 Files

```elisp
(require 'h5-io)

;; Create file
(let* ((file (h5-io-create "output.h5"))
       ;; Create a 1D dataset with 100 elements
       (ds (h5-io-create-dataset file "/temperature" '(100))))

  ;; Write data
  (let ((data (cl-loop for i from 0 below 100
                      collect (+ 20.0 (* i 0.1)))))
    (h5-io-write-dataset file ds data))

  ;; Add attributes
  (h5-io-write-attribute ds "units" "Celsius")
  (h5-io-write-attribute ds "description" "Temperature measurements")

  ;; Save and close
  (h5-io-save file)
  (h5-io-close file))
```

#### Creating Multi-dimensional Datasets

```elisp
;; Create a 10x10 2D dataset
(let* ((file (h5-io-create "matrix.h5"))
       (ds (h5-io-create-dataset file "/matrix" '(10 10))))

  ;; Generate and write 10x10 matrix (flattened)
  (let ((data (cl-loop for i from 0 below 100
                      collect (* i 0.5))))
    (h5-io-write-dataset file ds data))

  (h5-io-save file)
  (h5-io-close file))
```

### API Reference

#### File Operations

- `(h5-io-open filename)` - Open HDF5 file for reading
- `(h5-io-create filename)` - Create new HDF5 file for writing
- `(h5-io-close file)` - Close file and cleanup resources
- `(h5-io-save file)` - Save file to disk
- `(h5-io-walk file function)` - Walk through all objects in file

#### Dataset Operations

- `(h5-io-create-dataset file path dimensions &optional dtype)` - Create dataset
- `(h5-io-write-dataset file dataset data)` - Write data to dataset
- `(h5-io-read-dataset file path)` - Read dataset from file

#### Attribute Operations

- `(h5-io-write-attribute object name value)` - Write attribute to object
- Object can be a group or dataset

#### Data Structures

- `h5-io-file` - HDF5 file handle
- `h5-io-superblock` - File format superblock
- `h5-io-group` - Group object
- `h5-io-dataset` - Dataset object
- `h5-io-datatype` - Data type descriptor
- `h5-io-dataspace` - Dataspace (dimensions)
- `h5-io-attribute` - Attribute

### Examples

See `h5-examples.el` for comprehensive examples including:

1. Creating simple HDF5 files
2. Multi-dimensional datasets
3. Reading HDF5 files
4. Walking file structures
5. Multiple datasets in one file
6. Different data types
7. Batch processing files
8. API comparison with Go library

Run all examples:
```elisp
(require 'h5-examples)
(h5-example-run-all)
```

### Comparison with Go scigolib/hdf5

The API closely mirrors the Go library:

**Go:**
```go
file, err := hdf5.Open("data.h5")
defer file.Close()

file.Walk(func(path string, obj hdf5.Object) {
  switch v := obj.(type) {
  case *hdf5.Group:
    fmt.Printf("Group: %s\n", path)
  case *hdf5.Dataset:
    fmt.Printf("Dataset: %s\n", path)
  }
})
```

**Emacs Lisp:**
```elisp
(let ((file (h5-io-open "data.h5")))
  (unwind-protect
      (h5-io-walk file
                 (lambda (path obj)
                   (cond
                    ((h5-io-group-p obj)
                     (message "Group: %s" path))
                    ((h5-io-dataset-p obj)
                     (message "Dataset: %s" path)))))
    (h5-io-close file)))
```

### Implementation Status

#### Implemented
- ✅ Basic HDF5 superblock reading (version 0, 1, 2, 3)
- ✅ Binary I/O utilities (integers, floats, strings)
- ✅ Data structures for groups, datasets, attributes
- ✅ High-level read/write API
- ✅ File creation and superblock writing
- ✅ Dataset and attribute structures

#### Planned
- 🔄 Full object header parsing
- 🔄 Symbol table and B-tree structures
- 🔄 Chunked dataset layout
- 🔄 Compression support (GZIP)
- 🔄 Compound data types
- 🔄 Group hierarchy management

## h5-mode - Interactive HDF5 Viewer

## Installation

Add h5-el to your Emacs load path and require h5-mode:

```elisp
(add-to-list 'load-path "/path/to/h5-el")
(require 'h5-mode)
```

h5-mode will automatically activate when you open .h5 or .hdf5 files from dired or with `find-file`.

See `sample-emacs-config.el` for a complete configuration example.

## Usage

### From Dired

1. Navigate to a directory with HDF5 files (`C-x d`)
2. Move cursor to an .h5 or .hdf5 file
3. Press `RETURN`
4. The file will open in h5-mode showing only top-level groups (for faster loading)

### Direct Usage

```
M-x h5-mode
```

It's similar to Dired.

### Performance Optimization

**Critical for Large Files (e.g., 19GB files):**

h5-mode is optimized to handle very large HDF5 files efficiently:

1. **Partial File Reading**: Only reads the first 1MB of the file by default, not the entire file
   - This prevents Emacs from consuming excessive memory
   - Safe for files of any size (tested with 19GB+ files)

2. **Depth Control**: Displays only top-level groups initially
   - Fast initial loading showing just the root structure
   - Expand to deeper levels on demand (press 'a', '2', or '3')

3. **Customizable**: Control how much to read initially
   ```elisp
   ;; Read more if needed (e.g., 10MB for deeper metadata)
   (setq h5-initial-read-bytes (* 10 1024 1024))

   ;; Or less for ultra-fast loading (e.g., 512KB)
   (setq h5-initial-read-bytes (* 512 1024))
   ```

**Memory Usage**: Opening a 19GB file uses only ~1MB of memory instead of loading the entire file.

### View Everything (Datasets, Attributes, and Group)

#### Navigation
- `RET` - Open/view object at point (group, dataset, or attribute)
- `SPC` - Move to next line
- `q` - Quit window

#### View Depth Control (NEW)
- `a` - Show all levels (full tree)
- `1` - Show only top-level (depth 1) - **Default**
- `2` - Show up to depth 2
- `3` - Show up to depth 3
- `g` - Refresh display

#### Group Operations
- `+` - Create group
- `d` - Delete group
- `s` - Sort group

#### View Dataset as an Image
- `i` - View dataset as image
- `e` - Export dataset as image

#### View Dataset as Text
- `RET` - View dataset as text
- `E` - Export dataset as CSV

#### View Attribute
- `RET` - View attribute

#### Search Value
- `C-s` - Search for values in the file

### Example Workflow

```
1. Open dired: C-x d
2. Navigate to HDF5 file: my_data.h5
3. Press: RETURN
4. h5-mode opens showing:

   HDF5 File: /path/to/my_data.h5

   Superblock Version: 0
   Size of offsets: 8
   Size of lengths: 8

   Objects:
   (Showing depth: 1, press 'a' to show all)

   [Group] /
   [Group] /group1
   [Dataset] /dataset1 (100)

5. Press 'a' to see all levels
6. Press '1' to return to top-level view
7. Move to a dataset and press RET to view it
8. Press 'E' to export dataset to CSV
```

