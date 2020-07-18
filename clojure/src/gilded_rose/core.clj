(ns gilded-rose.core)

(def legendary-item-names
  ["Sulfuras, Hand Of Ragnaros"])

(def backstage-pass-item-names
  ["Backstage passes to a TAFKAL80ETC concert"])

(def valueless-after-expiry-item-names backstage-pass-item-names)

(def appreciates-item-names
  (concat
   ["Aged Brie"]
   backstage-pass-item-names))

(def conjured-item-names
  ["Conjured jam"])

(def max-appreciation 50)

(def min-depreciation 0)

(defn- legendary? [{item-name :name}]
  (some #{item-name} legendary-item-names))

(defn- expired? [{:keys [sell-in]}] (< sell-in 0))

(defn- valueless-after-expiry? [{item-name :name}]
  (some #{item-name} valueless-after-expiry-item-names))

(defn- appreciates? [{item-name :name}]
  (some #{item-name} appreciates-item-names))

(defn- depreciates? [item]
  (not (or (legendary? item)
           (appreciates? item))))

(defn- conjured? [{item-name :name}]
  (some #{item-name} conjured-item-names))

(defn- backstage-pass? [{item-name :name}]
  (some #{item-name} backstage-pass-item-names))

(defn- decrease-sell-in [item]
  (if (legendary? item)
    item
    (update item :sell-in dec)))

(defn- cap-appreciation [{:keys [quality] :as item}]
  (if (> quality max-appreciation)
    (assoc item :quality max-appreciation)
    item))

(defn- cap-depreciation [{:keys [quality] :as item}]
  (if (< quality min-depreciation)
    (assoc item :quality min-depreciation)
    item))

(defn- appreciate-backstage-pass
  [{:keys [sell-in] :as item}]
  (cond (and (>= sell-in 5) (< sell-in 10))
        (update item :quality #(+ 2 %))
        (and (>= sell-in 0) (< sell-in 5))
        (update item :quality #(+ 3 %))
        :else
        (update item :quality inc)))

(defn- modify-quality [item]
  (cond
    (and (expired? item) (valueless-after-expiry? item))
    (assoc item :quality 0)
    (appreciates? item)
    (cap-appreciation
     (if (backstage-pass? item)
       (appreciate-backstage-pass item)
       (update item :quality inc)))
    (depreciates? item)
    (cap-depreciation
     (let [depreciation (if (expired? item) 2 1)
           depreciation-factor (if (conjured? item) 2 1)]
       (update item :quality (fn [q] (- q (* depreciation-factor depreciation))))))
    :else
    item))

(defn update-quality [items]
  (->> items
       (map decrease-sell-in)
       (map modify-quality)))

(defn item [item-name, sell-in, quality]
  {:name item-name, :sell-in sell-in, :quality quality})

(def initial-inventory
  [(item "+5 Dexterity Vest" 10 20)
   (item "Aged Brie" 2 0)
   (item "Elixir of the Mongoose" 5 7)
   (item "Sulfuras, Hand Of Ragnaros" 0 80)
   (item "Backstage passes to a TAFKAL80ETC concert" 15 20)])

(def conjured-jam (item "Conjured jam" 30 23))

(def conjured-expired-jam (item "Conjured jam" 0 23))

(def updated-initial-inventory
  (conj initial-inventory
        conjured-jam
        conjured-expired-jam))

(def current-inventory initial-inventory)

(defn update-current-inventory
  ([] (update-current-inventory current-inventory))
  ([curr-inv]
   (update-quality curr-inv)))

(comment

  (let [expected
        (list
         {:name "+5 Dexterity Vest" :sell-in 9 :quality 19}
         {:name "Aged Brie" :sell-in 1 :quality 1}
         {:name "Elixir of the Mongoose" :sell-in 4 :quality 6}
         ;; {:name "Sulfuras, Hand Of Ragnaros" :sell-in -1 :quality 80}
         {:name "Sulfuras, Hand Of Ragnaros" :sell-in 0 :quality 80}
         {:name "Backstage passes to a TAFKAL80ETC concert" :sell-in 14
          :quality 21})
        output (update-current-inventory)]
    (= expected output))

  [updated-initial-inventory
   (update-current-inventory updated-initial-inventory)]

  ;; Informal tests

  (let [items [(item "Conjured jam" 3 3)]
        exp [3 1 0 0 0 0 0]
        days 6
        quality-history
        (->> days
             range
             (reduce
              (fn [invs _] (conj invs (update-current-inventory (last invs))))
              [items])
             (map (comp :quality first)))]
    (= exp quality-history))

  (let [items [(item "Aged Brie" 3 48)]
        exp [48 49 50 50 50 50 50]
        days 6
        quality-history
        (->> days
             range
             (reduce
              (fn [invs _] (conj invs (update-current-inventory (last invs))))
              [items])
             (map (comp :quality first)))]
    (= exp quality-history))

  (let [items [(item "Backstage passes to a TAFKAL80ETC concert" 11 30)]
        exp [30 31 33 35 37 39 41 44 47 50 50 50 0]
        days 12
        quality-history
        (->> days
             range
             (reduce
              (fn [invs _] (conj invs (update-current-inventory (last invs))))
              [items])
             (map (comp :quality first)))]
    (= exp quality-history))

)
