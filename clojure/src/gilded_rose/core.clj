(ns gilded-rose.core)

(defn update-quality-original [items]
  (map
   (fn[item] (cond
               (and (< (:sell-in item) 0) (= "Backstage passes to a TAFKAL80ETC concert" (:name item)))
               (merge item {:quality 0})
               (or (= (:name item) "Aged Brie") (= (:name item) "Backstage passes to a TAFKAL80ETC concert"))
               (if (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 5) (< (:sell-in item) 10))
                 (merge item {:quality (inc (inc (:quality item)))})
                 (if (and (= (:name item) "Backstage passes to a TAFKAL80ETC concert") (>= (:sell-in item) 0) (< (:sell-in item) 5))
                   (merge item {:quality (inc (inc (inc (:quality item))))})
                   (if (< (:quality item) 50)
                     (merge item {:quality (inc (:quality item))})
                     item)))
               (< (:sell-in item) 0)
               (if (= "Backstage passes to a TAFKAL80ETC concert" (:name item))
                 (merge item {:quality 0})
                 (if (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item)))
                   (merge item {:quality (- (:quality item) 2)})
                   item))
               (or (= "+5 Dexterity Vest" (:name item)) (= "Elixir of the Mongoose" (:name item)))
               (merge item {:quality (dec (:quality item))})
               :else item))
   (map (fn [item]
          (if (not= "Sulfuras, Hand of Ragnaros" (:name item))
            (merge item {:sell-in (dec (:sell-in item))})
            item))
        items)))

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

;; Test Code

(def summarize-fix-item (juxt :name (comp :quality cap-depreciation)))

(def summarize-item (juxt :name :quality))

(defn qualities-equal?
  "Returns true iff all of the qualities of the supplied inventory histories are
  identical. Note that is applies the cap-depreciation function to all qualities
  to remove negative values prior to the equality comparison. If unequal, returns
  a sequence of 4-ary vectors containing false, the index of the inventory, and
  the two summarized inventories that compare unequal"
  [hist-orig hist-new]
  (let [quals-orig (->> hist-orig (map (fn [inv] (map summarize-fix-item inv))))
        quals-new (->> hist-new (map (fn [inv] (map summarize-item inv))))
        are-equal (= quals-orig quals-new)]
    (or are-equal
        [are-equal
         (->> quals-new
              (interleave quals-orig)
              (partition 2)
              (map-indexed (fn [idx [orig-inv inv]]
                             [(= orig-inv inv) idx orig-inv inv]))
              (filter #(not (first %))))])))

(defn- get-quality-history
  "Given an initial inventory, an integer of days, and an update function, return
  a vector of inventories documenting the history of how the inventory has
  changed."
  [init-inv days updater]
  (->> days
       range
       (reduce
        (fn [invs _] (conj invs (updater (last invs))))
        [init-inv])))

(defn regression-test
  "Calling this function should return true if the new update quality function
  ``new-fn`` (which defaults to ``update-quality``) behaves the same as the
  untouched ``update-quality-original`` function."
  ([] (regression-test 100 update-quality))
  ([days] (regression-test days update-quality))
  ([days new-fn]
   (let [inventory initial-inventory
         quality-history-original (get-quality-history
                                   inventory days update-quality-original)
         quality-history (get-quality-history inventory days new-fn)]
     (qualities-equal? quality-history-original quality-history))))

(comment

  ;; Corrections made to the original code (aside from cosmetic refactorings)
  ;; 1. Prevent quality values from decreasing to values less than 0. Implemented
  ;;    via ``cap-depreciation`` function. This ensures that "the quality of an
  ;;    item is never negative"

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

  (regression-test)

)
