## Tiny-C Compiler in Common Lisp
* version 0.2.1

### 概要
* 計算機科学実験及演習3 (ソフトウェア) [1]

### 環境
* SBCL を想定

### 使用法 (インタプリタ上)
* 読込み
    * (ql:quickload :cl-tcc)
* (テストの実行)
    * (cl-tcc:run-test) ; Tiny-C のソースコードサンプルは test/*.tc
* 任意の Tiny-C ソースコードのコンパイル
    * (cl-tcc:tcc *.tc *.asm)
* (アセンブル, リンク)
    * nasm -f elf32 -o hoge.o hoge.asm
    * gcc -m32 -o hoge.out hoge.o
* (実行)
    * hoge.out

### 使用法 (実行形式として)
* 読込み
    * (ql:quickload :cl-tcc)
* 自身のコンパイル
    * (sb-ext:save-lisp-and-die "cl-tcc.out" :toplevel 'cl-tcc:main :executable t :purify t)
* 任意の Tiny-C ソースコードのコンパイル
    * ./cl-tcc.out hoge.tc -o hoge.asm
* (アセンブル, リンク)
    * nasm -f elf32 -o hoge.o hoge.asm
    * gcc -m32 -o hoge.out hoge.o
* (実行)
    * hoge.out

### リンク
* [1] http://www.fos.kuis.kyoto-u.ac.jp/~umatani/le3b/index.html
