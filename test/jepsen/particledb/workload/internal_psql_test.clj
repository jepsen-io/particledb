(ns jepsen.particledb.workload.internal-psql-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [jepsen.particledb.workload.internal-psql :refer :all]))

(deftest parse-test
  (testing "update, read"
    (is (= [[:update 0 48] [:r 0 48]]
           (parse [[:update 0 48] [:r 0 nil]]
                  "BEGIN\nUPDATE 1\n val \n-----\n  48\n(1 row)\n\nCOMMIT"))))
  (testing "update fails"
    (is (= [[:r 0 1]]
           (parse [[:update 0 48] [:r 0 nil]]
                  "BEGIN\nUPDATE 0\n val \n-----\n  1\n(1 row)\n\nCOMMIT"))))

  (testing "insert ok"
    (is (= [[:insert 3 4] [:r 0 2]]
           (parse [[:insert 3 4] [:r 0 nil]]
                  "BEGIN\nINSERT 0 1\n val \n-----\n  2\n(1 row)\n\nCOMMIT"))))

  (testing "empty read"
    (is (= [[:r 2 nil] [:update 2 14] [:r 2 14] [:r 2 14]]
           (parse [[:r 2 nil] [:update 2 14] [:r 2 nil] [:r 2 nil]]
             "BEGIN\nSELECT 0\nUPDATE 1\n val \n-----\n  14\n(1 row)\n\n val \n-----\n  14\n(1 row)\n\nCOMMIT"))))

  (testing "empty read 2"
    (is (= [[:r 2 nil] [:r 2 nil]]
           (parse [[:r 2 nil] [:r 2 nil]]
                  "BEGIN\n val \n-----\n(0 rows)\n\n val \n-----\n(0 rows)\n\nCOMMIT"))))

  (testing "rollback"
    (is (thrown? clojure.lang.ExceptionInfo
                 (parse [[:insert 1 2]]
                        "BEGIN\nROLLBACK"))))
  )
