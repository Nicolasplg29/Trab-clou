# 🧩 Exercício Integrador – Sistema de Notas e Estatísticas

## 🎯 Objetivo
Desenvolver um programa completo em **Clojure** que leia as notas de vários alunos, calcule estatísticas e classifique o desempenho geral da turma.


## 📋 Descrição
O programa deve:

1. Solicitar a **quantidade de alunos** da turma.  
2. Usar um **laço de repetição** (`loop` ou `dotimes`) para:
   - Pedir o **nome** e a **nota (0–100)** de cada aluno.  
   - Classificar a nota usando uma **estrutura condicional** (`cond`) conforme a tabela abaixo:
     - `>= 90` → A  
     - `>= 80` → B  
     - `>= 70` → C  
     - `>= 60` → D  
     - `< 60` → F  
   - Exibir o **conceito** do aluno na tela.  
3. Após o término das entradas, o programa deve:
   - Calcular e exibir a **média geral da turma**.  
   - Exibir a **quantidade de alunos aprovados** (nota ≥ 60).  
   - Exibir uma **mensagem de desempenho geral** utilizando `if`:  
     - Média ≥ 80 → “Turma excelente!”  
     - Média ≥ 60 → “Bom desempenho!”  
     - Caso contrário → “É necessário melhorar!”

---

## 🧠 Conceitos que devem ser utilizados
- `def` → para valores **globais** (limites, mensagens).  
- `let` → para **variáveis locais** dentro do loop.  
- `loop/recur` ou `dotimes` → para **repetição**.  
- `cond` ou `if` → para **classificação e decisão**.  
- Funções aritméticas básicas (`+`, `/`, `inc`).

---

## 📄 Exemplo de execução esperada

```
Quantos alunos na turma? 3

Nome do aluno 1: Ana
Nota: 95
Ana - Conceito: A

Nome do aluno 2: Bruno
Nota: 72
Bruno - Conceito: C

Nome do aluno 3: Carla
Nota: 55
Carla - Conceito: F

Média da turma: 74.0
Aprovados: 2
Desempenho geral: Bom desempenho!
```

---

## 📬 Entrega

- **Prazo final:** **14/10/2025 às 22h30**  
- **Forma de envio:** através do formulário disponível no link abaixo:  

👉 [Enviar exercício integrador – Clojure](https://forms.office.com/r/QGGfNtHPDn)

---

**Boa prática e código limpo!**
