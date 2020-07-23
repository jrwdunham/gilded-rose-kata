(ns gilded-rose.core-test
  (:require [gilded-rose.core :as sut]
            [clojure.test :as t]))

(t/deftest gilded-rose-test
  (let [days 100
        new-fn sut/update-quality
        inventory sut/initial-extended-inventory
        quality-history-original
        (sut/get-quality-history inventory days sut/update-quality-original)
        quality-history (sut/get-quality-history inventory days new-fn)]
    (t/is (true? (sut/qualities-equal? quality-history-original quality-history)))))

