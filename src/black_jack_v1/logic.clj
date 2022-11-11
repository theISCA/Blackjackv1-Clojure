(ns black-jack-v1.logic
  (:require [card-ascii-art.core :as card]))

; Função para gerar uma carta
; A 2 3 4 5 6 7 8 9 10 J Q K
; 1.......................13
(defn new-card
  "Gera uma carta entre os números 1 e 13"
  []
  (inc (rand-int 13)))

; Função para definir as cartas J Q K como valor 10
(defn JQK->10
  "Define as cartas J Q K com valor 10"
  [card]
  (if (> card 10)
    10
    card))

; Função para definir a carta A como valor 11
(defn A->11
  [card]
  (if (= card 1)
    11
    card))

; Função para calcular os pontos de acordo com as cartas
; J Q K = 10 pontos
; [A 10] = 11 ou 21
; [A 5 7] = + 1 5 7 = 13 ou + 11 5 7 = 23
; A = 11 porém se passar de 21, ele vai valer 1
(defn points-cards
  "Calcula os pontos das cartas"
  [cards]
  (let [cards-without-JQK (map JQK->10 cards)
        cards-with-A11 (map A->11 cards-without-JQK)
        points-with-A1 (reduce + cards-without-JQK)
        points-with-A11 (reduce + cards-with-A11)]
    (if (> points-with-A11 21)
      points-with-A1
      points-with-A11)))

; Como representar um jogador
; {:player "Gabriel Escareli"
;  :cards [X X]}
(defn player
  "Gera o jogador e suas cartas"
  [player-name]
  (let [card-1 (new-card)
        card-2 (new-card)
        cards [card-1 card-2]
        points (points-cards cards)]
    {:player-name player-name
     :cards       cards
     :points      points}))

; Função para gerar uma nova carta
; Atualiza o vetor cards dentro do player com a nova carta
; Atualiza os pontos com o novo vetor de cartas
(defn more-card
  "Gera mais cartas ao jogador"
  [player]
  (let [card (new-card)
        cards (conj (:cards player) card)
        new-player (update player :cards conj card)
        points (points-cards cards)]
    (assoc new-player :points points)))

; Função de decisão do jogador
(defn player-decision-continue?
  "Define a decisão do jogador"
  [player]
  (println (:player-name player) ": mais cartas?")
  (= (read-line) "sim"))

; Função de decisão do dealer
(defn dealaer-decision-continue?
  "Define a decisão do dealer"
  [player-points dealer]
  (let [dealer-points (:points dealer)]
    (if (> player-points 21)
      false
      (<= dealer-points player-points))))

; Função para perguntar ao jogador se ele quer mais carta
; Chama a função more-card
(defn game-logic
  "Pergunta ao jogar se ele quer mais cartas e atualiza"
  [player fn-decision-continue?]
  (if (fn-decision-continue? player)
    (let [player-with-more-cards (more-card player)]
      (card/print-player player-with-more-cards)
      (game-logic player-with-more-cards fn-decision-continue?))
    player))

; Se ambos passarem de 21 pontos -> ambos perderam
; Se os pontos são iguais -> empate
; Se o player passou de 21 pontos -> dealer ganhou
; Se o dealer passou de 21 pontos -> player ganhou
; Se os pontos do player maior que os pontos do dealer -> player ganhou
; Se os pontos do dealer maio que os pontos do player -> dealer ganhou
(defn end-game
  [player dealer]
  (let [player-points (:points player)
        dealer-points (:points dealer)
        player-name (:player-name player)
        dealer-name (:player-name dealer)
        message (cond
                  (and (> player-points 21) (> dealer-points 21)) "Ambos perderam!"
                  (= player-points dealer-points) "Empate!"
                  (> player-points 21) (str dealer-points " ganhou!")
                  (> dealer-points 21) (str player-name " ganhou!")
                  (> player-points dealer-points) (str player-name " ganhou!")
                  (> dealer-points player-points) (str dealer-name " ganhou!"))]
    (card/print-player player)
    (card/print-player dealer)
    (print message)))

(def player-1 (player "Gabriel Escareli"))
(card/print-player player-1)

(def dealer (player "Dealer"))
(card/print-masked-player dealer)

(def player-after-game(game-logic player-1 player-decision-continue?))
(def partial-dealer-continue? (partial dealaer-decision-continue? (:points player-after-game)))
(def dealer-after-game (game-logic dealer partial-dealer-continue?))

(end-game player-after-game dealer-after-game)
