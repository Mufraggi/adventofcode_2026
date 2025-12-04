Parfait ! Voici le setup idéal d’un repo Advent of Code en OCaml, généré proprement avec dune, avec :

un exécutable par jour (day01, day02, …)

un module de lib partagé (utils)

une structure claire pour stocker les inputs

une commande simple pour lancer chaque jour

🎄 Tu vas obtenir un squelette prêt à coder ton AoC 2024/2025.

✅ 1. Créer le dossier du projet
mkdir advent-of-code
cd advent-of-code

✅ 2. Créer le projet dune
dune init proj aoc


Cela te donne la structure classique.

On va maintenant l’adapter au format Advent of Code.

🎁 3. Structure recommandée

Voici ce que nous voulons obtenir :

advent-of-code/
├── dune-project
├── aoc.opam
├── bin/
│   ├── dune
│   ├── day01.ml
│   ├── day02.ml
│   └── ...
├── lib/
│   ├── dune
│   └── utils.ml
├── inputs/
│   ├── day01.txt
│   ├── day02.txt
│   └── ...
└── test/

✅ 4. Générer les fichiers dune pour les days
👉 bin/dune

Remplace ton bin/dune par :

(executables
 (names day01 day02)
 (libraries aoc))


➡️ Tu pourras ajouter day03 etc. au fur et à mesure.

🛠 5. Exemple minimal de fichiers
👉 bin/day01.ml
let () =
  let input = In_channel.read_all "inputs/day01.txt" in
  print_endline ("Day 01 — part 1: " ^ string_of_int (String.length input));
  print_endline ("Day 01 — part 2: TODO");

👉 bin/day02.ml
let () = print_endline "Day 02 — not implemented yet"

👉 lib/dune
(library
 (name aoc))

👉 lib/utils.ml
let read_input filename =
  In_channel.read_all ("inputs/" ^ filename)

🚀 6. Lancer un jour

Depuis la racine :

dune exec day01


ou

dune exec bin/day01.exe

🎄 7. Ajouter un nouveau jour automatiquement

Tu veux que je te génère une commande bash pour créer automatiquement :

bin/dayXX.ml

inputs/dayXX.txt

mettre à jour bin/dune

?

Ou bien je peux te générer un script dune ou une commande Makefile pour automatiser tout ça.

👉 Dis-moi si tu veux une automatisation !