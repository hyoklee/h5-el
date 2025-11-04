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

### Testing

Two test scripts are provided to verify the implementation:

#### verify-signature.el
Tests that the HDF5 file signature is written and read correctly:
```elisp
(load-file "verify-signature.el")
;; Creates a test file and verifies the signature bytes
```

#### test-h5-io.el
Tests the complete write/read cycle:
```elisp
(load-file "test-h5-io.el")
;; Creates a file with a dataset, saves it, then reads it back
```

### Implementation Status

#### Implemented
- ✅ Basic HDF5 superblock reading (version 0, 1, 2, 3)
- ✅ Binary I/O utilities (integers, floats, strings)
- ✅ Data structures for groups, datasets, attributes
- ✅ High-level read/write API
- ✅ File creation and superblock writing
- ✅ Dataset and attribute structures
- ✅ Correct binary file handling with proper encoding

#### Planned
- 🔄 Full object header parsing
- 🔄 Symbol table and B-tree structures
- 🔄 Chunked dataset layout
- 🔄 Compression support (GZIP)
- 🔄 Compound data types
- 🔄 Group hierarchy management

## h5-mode - Interactive HDF5 Viewer

## Usage
```
M-x h5-mode
```

It's similar to Dired.

### View Everything (Datasets, Attributes, and Group)
#### Collapse/Expand Group
`RET` open-group

`+` create-group

`d` delete-group

`s` sort-group


### View Dataset as an Image
`i` view-as-image

`e` export-as-image

### View Dataset as Text
`RET` view-as-text

`E` export-as-csv

### View Attribute
`RET` view-attribute

### Search Value
`Ctrl-s` search

### Quit
`q` quit-window


