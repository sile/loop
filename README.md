loop
====

* 関数型言語的かつ効率的にループを表現するためのライブラリ
* SBCLならLOOPマクロと同等程度の性能

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

1以上の平方根を取得 (最初の10個のみ):

    > (let ((tmp (loop:map #'sqrt (loop:from 1))))
        (loop:collect (loop:take 10 tmp)))
    => (1.0 1.4142135 1.7320508 2.0 2.236068 2.4494898 
        2.6457512 2.828427 3.0  3.1622777)

3の倍数の100以下の数値を合計値:
    
    ;; 1]
    > (let ((tmp (loop:filter (lambda (x) (zerop (mod x 3)))
                              (loop:from 1 :to 100))))
        (loop:sum #'identity tmp))
    => 1683
    
    ;; 2]
    > (loop:sum #'identity (loop:from 0 :to 100 :by 3))
    => 1683
    
    ;; 3]
    > (let ((tmp (loop:filter (lambda (x) (zerop (mod x 3)))
                              (loop:from 1 :to 100))))
        (loop:sum #'identity (loop:take-while (lambda (x) (<= x 100)) tmp)))
    => 1683

配列から添字が3か5の倍数の要素のみを、添字付きで取得する:

    > (let ((tmp (loop:zip (loop:from 0)
                           (loop:for-array #(:a :b :c :d :e :f :g :h :i :j :k :l :m)))))
        (loop:collect 
          (loop:map-n 2 #'list
            (loop:filter-n 2 (lambda (i elem)
                                (declare (ignore elem))
                                (or (zerop (mod i 3))
                                    (zerop (mod i 5))))
                             tmp))))
    => ((0 :A) (3 :D) (5 :F) (6 :G) (9 :J) (10 :K) (12 :M))

複雑なzipの例:

    > (let ((as (loop:map (lambda (x) (* x x x)) (loop:down-from 100000 :by 7)))
            (bs (loop:filter #'oddp (loop:repeat (lambda () (random 100000)))))
            (cs (loop:drop-while (lambda (x) (< x 100))
                  (loop:map #'sqrt (loop:from 1)))))
        (loop:collect
          (loop:take 10
            (loop:map-n 3 #'list
              (loop:filter-n 3 (lambda (a b c)
                                 (declare (ignore b c))
                                 (evenp a))
                               (loop:zip as bs cs))))))
    => ((1000000000000000 1971 100.0) (999580058797256 21931 100.01)
        (999160235178048 68251 100.02) (998740529125912 17779 100.03)
        (998320940624384 89861 100.03999) (997901469657000 5897 100.04999)
        (997482116207296 5569 100.05998) (997062880258808 10653 100.06998)
        (996643761795072 56669 100.07997) (996224760799624 96629 100.08996))
