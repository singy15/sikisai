
# Sikisai

## Description
Sikisai is lightweight graphics library which has simple API.
Sikisai depends on cl-opengl and draws with OpenGL.

We works for the following cases.

- Making a software used Graphics API without the knowledge of OpenGL
- Drawing pictures quickly

* Currently, Sikisai supports the API for 2D.

[Demonstration is here(sikisai-sample](https://github.com/singy15/sikisai-sample)

## Features

* Drawing basicaly figures
* Reading/Writing RAW pictures
* Writing strings
* Interactions with keyboard and  mouse

## Dependencies

Sikisai depends these librarie.

* cl-opengl
* cl-glu
* cl-glut

## Installation

You can use the Sikisai from Quicklisp and ASDF. Currently Sikisai *hasn't* been indexed yet.
To use the Sikisai, Clone this repository.

```
; Reading with QuickLisp(local-projects)
(ql:quickload :sikisai)
```

```
; Reading with ASDF
(asdf:load-system :sikisai)
```

```
; Add the Sikisai into dependencies of .asd file.
(defsystem sikisai-sample
  :depends-on (:cl-opengl :cl-glut :cl-glu :sikisai)
  ...
```

You can use the Sikisai with adding the path into the components of your `.asd` file because Sikisai built on single file

```
; Add path into the components of your `.asd` file
(defsystem sikisai-sample
  :depends-on (:cl-opengl :cl-glut :cl-glu)
  :components (
    (:module "lib"
      :components (
        (:file "sikisai")
      ...

```

## Usage

It's trivial sample.

```
(ql:quickload :sikisai)

(defclass window (sik:window) ())

(defmethod sik:user-display ((this window))
  (sik:textb "Hello world!" 100.0 100.0))

(sik:display-window (make-instance 'window))
```

