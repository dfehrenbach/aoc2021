(ns aoc2021.day16.core
  (:require [clojure.string :as string]
            [clojure.walk :as walk]))

(def hex->binary
  {\0  "0000" \1  "0001" \2  "0010" \3  "0011"
   \4  "0100" \5  "0101" \6  "0110" \7  "0111"
   \8  "1000" \9  "1001" \A  "1010" \B  "1011"
   \C  "1100" \D  "1101" \E  "1110" \F  "1111"})

(defn convert [hexstr]
  (reduce str (map hex->binary hexstr)))

(def input1
  (->> "src/aoc2021/day16/input1.txt"
       slurp
       convert))

(defn binstr->int [s]
  (read-string (str "2r" s)))

(defn groups->int [gs]
  (binstr->int (string/join (map #(string/join (drop 1 %)) gs))))

(declare parse-next-packet)

(defn parse-literal [binary-str] ;;or binary-seq
  (when-let [[match version type groups _ last-group] (re-find #"^([01]{3})(100)((1[01]{4})*)(0[01]{4})" (string/join binary-str))]
    (let [groups (conj (mapv string/join (partition 5 groups)) last-group)]
      {:match match
       :total-length (count match)
       :version (binstr->int version)
       :type (binstr->int type)
       :value (groups->int groups)
       :remaining-str (drop (count match) binary-str)})))

(defn parse-operator-zero [binary-str] ;;or binary-seq
  (when-let [[match version type length-type length] (re-find #"^([01]{3})([01]{3})(0)([01]{15})" (string/join binary-str))]
    {:match match
     :version (binstr->int version)
     :type (binstr->int type)
     :length-type (binstr->int length-type)
     :length (binstr->int length)
     :total-length (+ (count match) (binstr->int length))
     :remaining-str (drop (+ (count match) (binstr->int length)) binary-str)
     :subpackets (->> {:remaining-str (->> binary-str (drop (count match)) (take (binstr->int length)))}
                      (iterate parse-next-packet)
                      rest
                      (take-while some?))}))

(defn parse-operator-one [binary-str] ;;or binary-seq
  (when-let [[match version type length-type subpacket-num] (re-find #"^([01]{3})([01]{3})(1)([01]{11})" (string/join binary-str))]
    (let [subpackets (->> {:remaining-str (drop (count match) binary-str)}
                          (iterate parse-next-packet)
                          rest
                          (take (binstr->int subpacket-num)))
          subpacket-len (reduce + (map :total-length subpackets))]
      {:match match
       :version (binstr->int version)
       :type (binstr->int type)
       :length-type (binstr->int length-type)
       :total-length (+ (count match) subpacket-len)
       :subpacket-num (binstr->int subpacket-num)
       :remaining-str (drop (+ (count match) subpacket-len) binary-str)
       :subpackets subpackets})))

(defn parse-next-packet [{s :remaining-str}]
  (when (seq s)
    (condp apply [s]
      parse-literal :>> identity
      parse-operator-zero :>> identity
      parse-operator-one :>> identity
      (throw (ex-info s {:bad-string s})))))

(defn version-sum [packet-tree]
  (reduce + (map :version (set (tree-seq :subpackets :subpackets packet-tree)))))

(defn part1 [input]
  (->> {:remaining-str input}
       parse-next-packet
       version-sum))

(def operator->fn {0 +, 1 *, 2 min, 3 max
                   5 #(if (apply > %&) 1 0)
                   6 #(if (apply < %&) 1 0)
                   7 #(if (apply = %&) 1 0)})

(defn evaluate-ops [node]
  (cond
    (:subpackets node) (apply (operator->fn (:type node)) (:subpackets node))
    (:value node) (:value node)
    :else node))

(defn part2 [input]
  (->> {:remaining-str input}
       parse-next-packet
       (walk/postwalk evaluate-ops)))

(comment

  (def literal-value-test (convert "D2FE28"))
  (def operator-0-test (convert "38006F45291200"))
  (def operator-1-test (convert "EE00D40C823060"))
;; sum of 16
  (def test1 (convert "8A004A801A8002F478"))
;; sum of 12
  (def test2 (convert "620080001611562C8802118E34"))
;; sum of 23
  (def test3 (convert "C0015000016115A2E0802F182340"))
;; sum of 31
  (def test4 (convert "A0016C880162017C3686B18A3D4780"))

  (groups->int ["10111" "11110" "00101"])
  ;; => 2021

  (parse-literal literal-value-test)
  ;; => {:match "110100101111111000101", :version 6, :type 4, :groups ["10111" "11110" "00101"], :value 2021, :remaining-str "000"}

  (parse-operator-zero operator-0-test)
  ;; => {:match "0011100000000000011011", :version 1, :type 6, :length-type 0, :length 27, :subpacket-str "110100010100101001000100100", :remaining-str "0000000", :subpackets ({:match "11010001010", :version 6, :type 4, :groups ["01010"], :value 10, :remaining-str "0101001000100100"} {:match "0101001000100100", :version 2, :type 4, :groups ["10001" "00100"], :value 20, :remaining-str ""})}

  (parse-operator-one (str operator-1-test))
  ;; bug in remaining-str => {:match "111011100000000011", :version 7, :type 3, :length-type 1, :subpacket-num 3, :remaining-str "01010000001100100000100011000001100000", :subpackets ({:match "01010000001", :version 2, :type 4, :groups ["00001"], :value 1, :remaining-str "100100000100011000001100000"} {:match "10010000010", :version 4, :type 4, :groups ["00010"], :value 2, :remaining-str "0011000001100000"} {:match "00110000011", :version 1, :type 4, :groups ["00011"], :value 3, :remaining-str "00000"})}
  ;; fixed => {:match "111011100000000011", :version 7, :type 3, :length-type 1, :subpacket-num 3, :remaining-str "00000", :subpackets ({:match "01010000001", :version 2, :type 4, :groups ["00001"], :value 1, :remaining-str "100100000100011000001100000"} {:match "10010000010", :version 4, :type 4, :groups ["00010"], :value 2, :remaining-str "0011000001100000"} {:match "00110000011", :version 1, :type 4, :groups ["00011"], :value 3, :remaining-str "00000"})}

  (parse-next-packet {:remaining-str test1})
  ;; => {:match "100010100000000001", :version 4, :type 2, :length-type 1, :subpacket-num 1, :remaining-str "001010100000000001101010000000000000101111010001111000", :subpackets ({:match "001010100000000001", :version 1, :type 2, :length-type 1, :subpacket-num 1, :remaining-str "101010000000000000101111010001111000", :subpackets ({:match "1010100000000000001011", :version 5, :type 2, :length-type 0, :length 11, :subpacket-str "11010001111", :remaining-str "000", :subpackets ({:match "11010001111", :version 6, :type 4, :groups ["01111"], :value 15, :remaining-str ""})})})}
  (version-sum (parse-next-packet {:remaining-str test1}))
  ;; => 16


  (parse-next-packet {:remaining-str test2})
  ;; => {:match "011000100000000010", :version 3, :type 0, :length-type 1, :subpacket-num 2, :remaining-str "00000000000000000101100001000101010110001011001000100000000010000100011000111000110100", :subpackets ({:match "0000000000000000010110", :version 0, :type 0, :length-type 0, :length 22, :subpacket-str "0001000101010110001011", :remaining-str "001000100000000010000100011000111000110100", :subpackets ({:match "00010001010", :version 0, :type 4, :groups ["01010"], :value 10, :remaining-str "10110001011"} {:match "10110001011", :version 5, :type 4, :groups ["01011"], :value 11, :remaining-str ""})} {:match "001000100000000010", :version 1, :type 0, :length-type 1, :subpacket-num 2, :remaining-str "000100011000111000110100", :subpackets ({:match "00010001100", :version 0, :type 4, :groups ["01100"], :value 12, :remaining-str "0111000110100"} {:match "01110001101", :version 3, :type 4, :groups ["01101"], :value 13, :remaining-str "00"})})}
  (version-sum (parse-next-packet {:remaining-str test2}))
  ;; => 12


  (parse-next-packet {:remaining-str test3})
  ;; => {:match "1100000000000001010100", :version 6, :type 0, :length-type 0, :length 84, :subpacket-str "000000000000000001011000010001010110100010111000001000000000101111000110000010001101", :remaining-str "000000", :subpackets ({:match "0000000000000000010110", :version 0, :type 0, :length-type 0, :length 22, :subpacket-str "0001000101011010001011", :remaining-str "1000001000000000101111000110000010001101", :subpackets ({:match "00010001010", :version 0, :type 4, :groups ["01010"], :value 10, :remaining-str "11010001011"} {:match "11010001011", :version 6, :type 4, :groups ["01011"], :value 11, :remaining-str ""})} {:match "100000100000000010", :version 4, :type 0, :length-type 1, :subpacket-num 2, :remaining-str "1111000110000010001101", :subpackets ({:match "11110001100", :version 7, :type 4, :groups ["01100"], :value 12, :remaining-str "00010001101"} {:match "00010001101", :version 0, :type 4, :groups ["01101"], :value 13, :remaining-str ""})} {:match "11110001100", :version 7, :type 4, :groups ["01100"], :value 12, :remaining-str "00010001101"} {:match "00010001101", :version 0, :type 4, :groups ["01101"], :value 13, :remaining-str ""})}
  (version-sum (parse-next-packet {:remaining-str test3}))
  ;; => 23

  (parse-next-packet {:remaining-str test4})
  ;; => {:match "1010000000000001011011", :version 5, :type 0, :length-type 0, :length 91, :subpacket-str "0010001000000000010110001000000001011111000011011010000110101100011000101000111101010001111", :remaining-str "0000000", :subpackets ({:match "001000100000000001", :version 1, :type 0, :length-type 1, :subpacket-num 1, :remaining-str "0110001000000001011111000011011010000110101100011000101000111101010001111", :subpackets ({:match "011000100000000101", :version 3, :type 0, :length-type 1, :subpacket-num 5, :remaining-str "1111000011011010000110101100011000101000111101010001111", :subpackets ({:match "11110000110", :version 7, :type 4, :groups ["00110"], :value 6, :remaining-str "11010000110101100011000101000111101010001111"} {:match "11010000110", :version 6, :type 4, :groups ["00110"], :value 6, :remaining-str "101100011000101000111101010001111"} {:match "10110001100", :version 5, :type 4, :groups ["01100"], :value 12, :remaining-str "0101000111101010001111"} {:match "01010001111", :version 2, :type 4, :groups ["01111"], :value 15, :remaining-str "01010001111"} {:match "01010001111", :version 2, :type 4, :groups ["01111"], :value 15, :remaining-str ""})})} {:match "011000100000000101", :version 3, :type 0, :length-type 1, :subpacket-num 5, :remaining-str "1111000011011010000110101100011000101000111101010001111", :subpackets ({:match "11110000110", :version 7, :type 4, :groups ["00110"], :value 6, :remaining-str "11010000110101100011000101000111101010001111"} {:match "11010000110", :version 6, :type 4, :groups ["00110"], :value 6, :remaining-str "101100011000101000111101010001111"} {:match "10110001100", :version 5, :type 4, :groups ["01100"], :value 12, :remaining-str "0101000111101010001111"} {:match "01010001111", :version 2, :type 4, :groups ["01111"], :value 15, :remaining-str "01010001111"} {:match "01010001111", :version 2, :type 4, :groups ["01111"], :value 15, :remaining-str ""})} {:match "11110000110", :version 7, :type 4, :groups ["00110"], :value 6, :remaining-str "11010000110101100011000101000111101010001111"} {:match "11010000110", :version 6, :type 4, :groups ["00110"], :value 6, :remaining-str "101100011000101000111101010001111"} {:match "10110001100", :version 5, :type 4, :groups ["01100"], :value 12, :remaining-str "0101000111101010001111"} {:match "01010001111", :version 2, :type 4, :groups ["01111"], :value 15, :remaining-str "01010001111"} {:match "01010001111", :version 2, :type 4, :groups ["01111"], :value 15, :remaining-str ""})}
  (version-sum (parse-next-packet {:remaining-str test4}))
  ;; => 31

  (part1 input1)
  ;; => 866

  (def test5 (convert "C200B40A82"))
  ;; finds the sum of 1 and 2, resulting in the value 3.
  (part2 test5)
  ;; => 3

  (def test6 (convert "04005AC33890"))
  ;; finds the product of 6 and 9, resulting in the value 54.
  (part2 test6)
  ;; => 54

  (def test7 (convert "880086C3E88112"))
  ;;finds the minimum of 7, 8, and 9, resulting in the value 7.
  (part2 test7)
  ;; => 7

  (def test8 (convert "CE00C43D881120"))
  ;; finds the maximum of 7, 8, and 9, resulting in the value 9
  (part2 test8)
  ;; => 9

  (def test9 (convert "D8005AC2A8F0"))
  ;; produces 1, because 5 is less than 15.
  (part2 test9)

  (def test10 (convert "F600BC2D8F"))
  ;; produces 0, because 5 is not greater than 15.
  (part2 test10)
  ;; => 0

  (def test11 (convert "9C005AC2F8F0"))
  ;; produces 0, because 5 is not equal to 15.
  (part2 test11)
  ;; => 0

  (def test12 (convert "9C0141080250320F1802104A08"))
  ;; produces 1, because 1 + 3 = 2 * 2.
  (part2 test12)
  ;; => 1

  (part2 input1)
  ;; => 1392637195518
  )
