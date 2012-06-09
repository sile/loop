loop
====

* 関数型言語的かつ効率的にループを表現するためのライブラリ

# サンプルコード
1から5の範囲のループ:
 > (loop:each (lambda (x) (print x))
              (loop:from 1 to 5))
 1
 2
 3
 4
 5
 => nil
 
配列の要素を集める:
 > (loop:collect (loop:for-array #(1 :two "TRHEE")))
 => (1 :TWO "TRHEE")
 
