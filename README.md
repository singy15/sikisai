# Mir

## 概要

Mirは軽量でシンプルなグラフィックスライブラリです。  
現在は2Dのみ対応しています。

## 依存関係

Mirは以下のライブラリに依存しています。

* cl-opengl
* cl-glu
* cl-glut

## インストール方法

MirはQuickLispとASDFから読み込むことができます、現時点ではMirはQuickLispのリポジトリに登録されて**いません**。  
リポジトリをcloneして利用してください。

```
; QuickLispを使用して読み込み (local-projects)
(ql:quickload :mir)
```

```
; ASDFを使用して読み込み
(asdf:load-system :mir)
```

```
; asdファイルの依存関係に追加
(defsystem mir-sample
  :depends-on (:cl-opengl :cl-glut :cl-glu :mir)
  ...
```

Mirは1ファイルで構成されているのでasdファイルのcomponentsに追加するだけでも動作します。

```
; asdファイルのコンポーネントに追加
(defsystem mir-sample
  :depends-on (:cl-opengl :cl-glut :cl-glu)
  :components (
    (:module "lib"
      :components (
        (:file "mir")
      ...

```

## 使用方法

簡単なサンプル。

```
(ql:quickload :mir)

(defclass window (mir:window) ())

(defmethod mir:user-display ((this window))
  (mir:textb "Hello world!" 100.0 100.0))

(mir:display-window (make-instance 'window))
```

