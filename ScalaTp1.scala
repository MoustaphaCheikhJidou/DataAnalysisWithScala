object ScalaTp1 {

  import scala.io.Source
  import scala.collection.mutable.ArrayBuffer
  import java.io.{BufferedWriter, File, FileWriter}

  // === Exercice 0 ===

  // Exercice 0 - Question 1 : Charger le fichier dans un Map
  def exo0_q1(filePath: String): Map[Int, Array[Array[Int]]] = {
    val lines = Source.fromFile(filePath).getLines().toArray
    val puzzles = ArrayBuffer[Array[Array[Int]]]()
    val puzzleBuffer = ArrayBuffer[Array[Int]]()
    var currentPuzzleIndex = 0

    for (line <- lines) {
      if (line.trim.forall(_ == '-')) {
        if (puzzleBuffer.length == 9) {
          puzzles.append(puzzleBuffer.toArray)
          puzzleBuffer.clear()
          currentPuzzleIndex += 1
        } else {
          throw new Exception(s"Format incorrect : puzzle $currentPuzzleIndex incomplet (${puzzleBuffer.length} lignes).")
        }
      } else if (line.trim.nonEmpty) {
        val row = line.trim.split("\\s+").map(_.toInt)
        puzzleBuffer.append(row)
      }
    }

    if (puzzleBuffer.nonEmpty) puzzles.append(puzzleBuffer.toArray)
    puzzles.zipWithIndex.map { case (puzzle, idx) => idx -> puzzle }.toMap
  }

  // Exercice 0 - Question 2 : Tester si un puzzle est valide
  def exo0_q2(puzzle: Array[Array[Int]]): Boolean = {
    def isValidGroup(group: Array[Int]): Boolean = {
      val filtered = group.filter(_ != 0)
      filtered.distinct.length == filtered.length
    }
    val rowsValid = puzzle.forall(isValidGroup)
    val colsValid = puzzle.transpose.forall(isValidGroup)
    val blocksValid = (0 until 3).flatMap { rowBlock =>
      (0 until 3).map { colBlock =>
        val block = for {
          i <- 0 until 3
          j <- 0 until 3
        } yield puzzle(rowBlock * 3 + i)(colBlock * 3 + j)
        isValidGroup(block.toArray)
      }
    }.forall(identity)
    rowsValid && colsValid && blocksValid
  }

  // Exercice 0 - Question 3 : Solutions possibles pour une cellule
  def exo0_q3(puzzle: Array[Array[Int]], row: Int, col: Int): List[Int] = {
    if (puzzle(row)(col) != 0) return List()
    val rowValues = puzzle(row)
    val colValues = puzzle.map(_(col))
    val blockValues = for {
      i <- 0 until 3
      j <- 0 until 3
    } yield puzzle((row / 3) * 3 + i)((col / 3) * 3 + j)
    val usedValues = rowValues ++ colValues ++ blockValues
    (1 to 9).filterNot(usedValues.contains).toList
  }

  // === Exercice 1 ===

  // Exercice 1 - Question 1 : Charger le fichier CSV
  def exo1_q1(filePath: String): List[Map[String, String]] = {
    val lines = Source.fromFile(filePath).getLines().toList
    val headers = lines.head.split(",").map(_.trim)
    lines.tail.map(line => headers.zip(line.split(",").map(_.trim)).toMap)
  }

  // Exercice 1 - Question 2 : Votes par Wilaya
  def exo1_q2(data: List[Map[String, String]]): Map[String, Map[String, Int]] = {
    data.groupBy(_("Wilaya")).view.mapValues { rows =>
      rows.groupBy(_("Candidat")).view.mapValues(_.map(_("nb_vote").toInt).sum).toMap
    }.toMap
  }

  // Exercice 1 - Question 3 : Pourcentages par Wilaya
  def exo1_q3(votesParWilaya: Map[String, Map[String, Int]]): Map[String, Map[String, Double]] = {
    votesParWilaya.map { case (wilaya, votes) =>
      val totalVotes = votes.values.sum.toDouble
      wilaya -> votes.map { case (candidat, nbVote) => candidat -> (nbVote / totalVotes * 100) }
    }
  }

  // Exercice 1 - Question 4 : Pourcentages globaux
  def exo1_q4(votesParWilaya: Map[String, Map[String, Int]]): Map[String, Double] = {
    val totalVotes = votesParWilaya.values.flatMap(_.values).sum.toDouble
    votesParWilaya.values.flatten.groupBy(_._1).map { case (candidat, votes) =>
      candidat -> (votes.map(_._2).sum / totalVotes * 100)
    }
  }

  // === Exercice 2 ===

  // Exercice 2 - Question 1 : Charger le fichier texte
  def exo2_q1(filePath: String): Array[(Int, String)] = {
    Source.fromFile(filePath).getLines().zipWithIndex.map { case (line, idx) => (idx + 1, line) }.toArray
  }

  // Exercice 2 - Question 2 : Liste des mots avec leurs numéros de ligne
  def exo2_q2(lines: Array[(Int, String)]): Array[(Int, String)] = {
    lines.flatMap { case (lineNumber, text) =>
      text.split("\\s+").filter(_.nonEmpty).map(word => (lineNumber, word))
    }
  }

  // Exercice 2 - Question 3 : Nettoyer les mots
  def exo2_q3(words: Array[(Int, String)]): Array[(Int, String)] = {
    words.map { case (lineNumber, word) =>
      (lineNumber, word.replaceAll("[^a-zA-Z0-9]", "").toLowerCase)
    }.filter { case (_, word) => word.nonEmpty }
  }

  // Exercice 2 - Question 4 : Générer l'index
  def exo2_q4(cleanedWords: Array[(Int, String)]): Map[String, List[Int]] = {
    cleanedWords.groupBy(_._2).view.mapValues(_.map(_._1).toList.distinct).toMap
  }

  // Exercice 2 - Question 5 : Écrire l'index dans un fichier
  def exo2_q5(index: Map[String, List[Int]], outputFilePath: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(new File(outputFilePath)))
    try {
      index.toSeq.sortBy(_._1).foreach { case (word, lines) =>
        writer.write(s"$word: ${lines.mkString(", ")}\n")
      }
    } finally {
      writer.close()
    }
  }

  // Exercice 2 - Question 6 : Fonction combinée
  def exo2_q6(inputFilePath: String, outputFilePath: String): Unit = {
    val linesWithNumbers = exo2_q1(inputFilePath)
    val wordsWithNumbers = exo2_q2(linesWithNumbers)
    val cleanedWords = exo2_q3(wordsWithNumbers)
    val wordIndex = exo2_q4(cleanedWords)
    exo2_q5(wordIndex, outputFilePath)
    println(s"Index écrit dans le fichier : $outputFilePath")
  }

  // === Fonction principale ===
def main(args: Array[String]): Unit = {
  try {
    println("=== Exercice 0 ===")

    // Exercice 0 - Question 1 : Charger les puzzles
    val filePathEx0 = "tp1-exo0-sudoku.txt"
    val puzzles = exo0_q1(filePathEx0)
    println("\nQuestion 1 : Puzzles chargés")
    puzzles.foreach { case (index, puzzle) =>
      println(s"\nPuzzle $index :")
      puzzle.foreach(row => println(row.mkString(" ")))
    }

    // Exercice 0 - Question 2 : Tester la validité des puzzles
    println("\nQuestion 2 : Validité des puzzles")
    puzzles.foreach { case (index, puzzle) =>
      val isValid = exo0_q2(puzzle)
      println(s"Puzzle $index est valide : $isValid")
    }

    // Exercice 0 - Question 3 : Solutions possibles pour une cellule
    println("\nQuestion 3 : Solutions possibles pour la première cellule vide de chaque puzzle")
    puzzles.foreach { case (index, puzzle) =>
      val solutions = exo0_q3(puzzle, 0, 0) // Cellule (0, 0)
      println(s"Puzzle $index : ${solutions.mkString(", ")}")
    }

    println("\n=== Exercice 1 ===")

    // Exercice 1 - Question 1 : Charger les données CSV
    val filePathEx1 = "resultat_presidentiel_2024.csv"
    val data = exo1_q1(filePathEx1)
    println("\nQuestion 1 : Données chargées")
    println(s"${data.take(5).mkString("\n")}")

    // Exercice 1 - Question 2 : Votes par Wilaya
    println("\nQuestion 2 : Votes par Wilaya")
    val votesParWilaya = exo1_q2(data)
    votesParWilaya.foreach { case (wilaya, votes) =>
      println(s"Wilaya $wilaya : $votes")
    }

    // Exercice 1 - Question 3 : Pourcentages par Wilaya
    println("\nQuestion 3 : Pourcentages par Wilaya")
    val pourcentagesParWilaya = exo1_q3(votesParWilaya)
    pourcentagesParWilaya.foreach { case (wilaya, pourcentages) =>
      println(s"Wilaya $wilaya : $pourcentages")
    }

    // Exercice 1 - Question 4 : Pourcentages globaux
    println("\nQuestion 4 : Pourcentages globaux")
    val pourcentagesGlobaux = exo1_q4(votesParWilaya)
    pourcentagesGlobaux.foreach { case (candidat, pourcentage) =>
      println(f"$candidat : $pourcentage%.2f %%")
    }

    println("\n=== Exercice 2 ===")

    // Exercice 2 - Question 1 : Charger le fichier texte
    val filePathEx2 = "merchantofvenice.txt"
    val linesWithNumbers = exo2_q1(filePathEx2)
    println("\nQuestion 1 : Fichier chargé avec numéros de ligne")
    linesWithNumbers.take(10).foreach(println)

    // Exercice 2 - Question 2 : Liste des mots avec numéros de ligne
    val wordsWithNumbers = exo2_q2(linesWithNumbers)
    println("\nQuestion 2 : Liste des mots avec numéros de ligne")
    wordsWithNumbers.take(10).foreach(println)

    // Exercice 2 - Question 3 : Nettoyer les mots
    val cleanedWords = exo2_q3(wordsWithNumbers)
    println("\nQuestion 3 : Mots nettoyés avec numéros de ligne")
    cleanedWords.take(10).foreach(println)

    // Exercice 2 - Question 4 : Générer l'index
    val wordIndex = exo2_q4(cleanedWords)
    println("\nQuestion 4 : Index des mots généré")
    wordIndex.take(10).foreach { case (word, lines) =>
      println(s"$word: ${lines.mkString(", ")}")
    }

    // Exercice 2 - Question 5 : Écrire l'index dans un fichier
    val outputFilePathEx2 = filePathEx2 + ".index"
    exo2_q5(wordIndex, outputFilePathEx2)
    println(s"\nQuestion 5 : Index écrit dans le fichier : $outputFilePathEx2")

    // Exercice 2 - Question 6 : Combiner tout
    println("\nQuestion 6 : Générer et écrire l'index avec la fonction combinée")
    exo2_q6(filePathEx2, outputFilePathEx2)

  } catch {
    case e: Exception =>
      println(s"Erreur : ${e.getMessage}")
  }
}

}
