
# Mir

## Description
Mir is lightweight graphics library which has simple API.
Mir depends on cl-opengl and draws with OpenGL.

We works for the following cases.

- Making a software used Graphics API without the knowledge of OpenGL
- Drawing pictures quickly

* Curreyntly, Mir supports the API for 2D.

[Demonstration is here(mir-sample](https://github.com/singy15/mir-sample)

## Features

* Drawing basicaly figures
* Reading/Writing RAW pictures
* Writing strings
* Interactions with keyboard and  mouse

## Dependencies

Mir depends these librarie.

* cl-opengl
* cl-glu
* cl-glut

## Installation

You can use the Mir from Quicklisp and ASDF. Currently Mir *hasn't* been indexed yet.
To use the Mir, Clone this repository.

```
; Reading with QuickLisp(local-projects)
(ql:quickload :mir)
```

```
; Reading with ASDF
(asdf:load-system :mir)
```

```
; Add the Mir into dependencies of .asd file.
(defsystem mir-sample
  :depends-on (:cl-opengl :cl-glut :cl-glu :mir)
  ...
```

You can use the Mir with adding the path into the components of your `.asd` file because Mir built on single file

```
; Add path into the components of your `.asd` file
(defsystem mir-sample
  :depends-on (:cl-opengl :cl-glut :cl-glu)
  :components (
    (:module "lib"
      :components (
        (:file "mir")
      ...

```

## Usage

It's trivial sample.

```
(ql:quickload :mir)

(defclass window (mir:window) ())

(defmethod mir:user-display ((this window))
  (mir:textb "Hello world!" 100.0 100.0))

(mir:display-window (make-instance 'window))
```

