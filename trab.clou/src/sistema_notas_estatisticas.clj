#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns sistema-notas-estatisticas
  "Programa Clojure para leitura de notas, classificação e cálculo de estatísticas de turma."
  (:gen-class))


(def nota-minima-aprovacao 60)
(def nota-conceito-a 90)
(def nota-conceito-b 80)
(def nota-conceito-c 70)

(def media-turma-excelente 80)
(def media-turma-boa 60)


(defn classifica-nota
  "Classifica a nota (0-100) em um conceito (A, B, C, D, F) usando `cond`."
  [nota]
  (cond
    (>= nota nota-conceito-a) "A"
    (>= nota nota-conceito-b) "B"
    (>= nota nota-conceito-c) "C"
    (>= nota nota-minima-aprovacao) "D"
    :else "F"))

(defn desempenho-geral
  "Determina a mensagem de desempenho geral da turma usando `if` aninhado."
  [media]
  (if (>= media media-turma-excelente)
    "Turma excelente!"
    (if (>= media media-turma-boa)
      "Bom desempenho!"
      "É necessário melhorar!")))


(defn -main
  "Função principal que executa o fluxo do programa."
  []
  (println "Quantos alunos na turma?")
  (flush)
  (let [num-alunos (try
                     (Integer/parseInt (read-line))
                     (catch NumberFormatException _ 0))]

    (if (<= num-alunos 0)
      (println "Quantidade de alunos inválida. Encerrando.")
      
      (let [resultados
            (loop [aluno-atual 1
                   total-notas 0
                   aprovados 0]

              (if (<= aluno-atual num-alunos)
                (do
                  (let [nome (do
                               (print (str "\nNome do aluno " aluno-atual ": "))
                               (flush)
                               (read-line))
                        nota (do
                               (print "Nota: ")
                               (flush)
                               (try
                                 (Integer/parseInt (read-line))
                                 (catch NumberFormatException _ -1)))
                        nota-valida (max 0 (min 100 nota))
                        conceito (classifica-nota nota-valida)
                        aluno-aprovado? (>= nota-valida nota-minima-aprovacao)]

                    (println (str nome " - Conceito: " conceito))

                    (recur (inc aluno-atual)
                           (+ total-notas nota-valida)
                           (if aluno-aprovado? (inc aprovados) aprovados))))
                {:total-notas total-notas
                 :aprovados aprovados
                 :num-alunos num-alunos}))]

        (let [media-geral (/ (double (:total-notas resultados))
                             (:num-alunos resultados))
              num-aprovados (:aprovados resultados)
              desempenho (desempenho-geral media-geral)]

          (println "\n" (apply str (repeat 25 "-")))
          (println (str "Média da turma: " (format "%.1f" media-geral)))
          (println (str "Aprovados: " num-aprovados))
          (println (str "Desempenho geral: " desempenho))
          (println (apply str (repeat 25 "-"))))))))

