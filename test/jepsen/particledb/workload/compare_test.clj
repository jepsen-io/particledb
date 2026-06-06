(ns jepsen.particledb.workload.compare-test
  (:require [clojure [pprint :refer [pprint]]
                     [test :refer :all]]
            [jepsen.particledb.workload.compare :as c]))

(deftest equiv-mop?-test
  (testing "unordered"
    (testing "same rows"
      (is (true? (c/equiv-mop?
                   {:sql "SELECT"
                    :pg [{:a (float 1.23) :b "bar"}
                         {:a (float 3.45) :b nil}]
                    :pdb [{:a 3.45, :b nil}
                          {:a 1.23, :b "bar"}]})))
      (testing "diff rows"
        (is (false?
              (c/equiv-mop?
                {:sql "SELECT"
                 :pg [{:a (float 1.23) :b "bar"}
                      {:a (float 5.45) :b nil}]
                 :pdb [{:a 3.45, :b nil}
                       {:a 1.23, :b "bar"}]}))))))

  (testing "ordered"
    (testing "same rows and order"
      (is (true?
            (c/equiv-mop?
              {:sql "SELECT ... ORDER BY a"
               :pg [{:a 1.23 :b "bar"}
                    {:a 3.45 :b nil}]
               :pdb [{:a 1.23, :b "bar"}
                     {:a 3.45, :b nil}]})))
      (testing "same rows, diff order"
        (is (false?
              (c/equiv-mop?
                {:sql "SELECT ... ORDER BY a"
                 :pg [{:a (float 1.23) :b "bar"}
                      {:a (float 5.45) :b nil}]
                 :pdb [{:a 1.23, :b "bar"}
                       {:a 3.45, :b nil}]})))))))

