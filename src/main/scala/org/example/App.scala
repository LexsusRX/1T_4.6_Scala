package org.example

/**
 * Напишите программу, которая:
 * выводит фразу «Hello, Scala!» справа налево
 * переводит всю фразу в нижний регистр
 * удаляет символ!
 * добавляет в конец фразы «and goodbye python!»
 *
 * Напишите программу, которая вычисляет ежемесячный оклад сотрудника после вычета налогов.
 * На вход вашей программе подается значение годового дохода до вычета налогов, размер премии  — в процентах от годового дохода и компенсация питания.
 *
 * Напишите программу, которая рассчитывает для каждого сотрудника отклонение (в процентах)
 * от среднего значения оклада на уровне всего отдела. В итоговом значении должно учитываться
 * в большую или меньшую сторону отклонение размера оклада. На вход вашей программе подаются все значения,
 * аналогичные предыдущей программе, а также список со значениями окладов сотрудников отдела 100, 150, 200, 80, 120, 75.
 *
 * Попробуйте рассчитать новую зарплату сотрудника, добавив (или отняв, если сотрудник плохо себя вел)
 * необходимую сумму с учетом результатов прошлого задания. Добавьте его зарплату в список и вычислите значение самой высокой зарплаты и самой низкой.
 *
 * Также в вашу команду пришли два специалиста с окладами 350 и 90 тысяч рублей.
 * Попробуйте отсортировать список сотрудников по уровню оклада от меньшего к большему.
 *
 * Кажется, вы взяли в вашу команду еще одного сотрудника и предложили ему оклад 130 тысяч.
 * Вычислите самостоятельно номер сотрудника в списке так, чтобы сортировка не нарушилась, и добавьте его на это место.
 *
 * Попробуйте вывести номера сотрудников из полученного списка, которые попадают под категорию middle.
 * На входе программе подается «вилка» зарплаты специалистов уровня middle.
 *
 * Однако наступил кризис и ваши сотрудники требуют повысить зарплату.
 * Вам необходимо проиндексировать зарплату каждого сотрудника на уровень инфляции  — 7 %.
 */
object App {


  def main(args: Array[String]) {

    words() //пункт а
    monthlySalary(40000, 70, 10, 12) //пункт b
    val salaryAll = List(100.6, 150.8, 200.55, 80, 120, 75)
    salaryDeviation(salaryAll) //пункт c
    val correctionSalary = List(1.1, 1, 0.85, 1.5, 1.3, 0.92)
    correctionBonus(salaryAll, correctionSalary) //пункт d
    val newEmployees = List(350.0, 90.0)
    sortedWithNewEmpl(salaryAll, newEmployees) //пункт e
    val newEmployee = 130.0
    addNewEmployeeWithSorted(salaryAll, newEmployees, newEmployee) //пункт f
    val rangeMin = 150.0
    val rangeMax = 250.0
    numberOfEmployeeOfMidle(salaryAll, newEmployees, newEmployee, rangeMin, rangeMax) //пункт g
    val percentIndexing = 7.0
    indexingSalary(salaryAll, newEmployees, newEmployee, percentIndexing) //пункт h


  }

  def words(): Unit = {
    //    выводит фразу «Hello, Scala !», справа налево
    //      переводит всю фразу в нижний регистр
    //      удаляет символ !добавляет в конец фразы «and goodbye python !»
    val word = "Hello, Scala!"
    val addedFrase = "and goodbye python!"
    println(word)
    println(word.reverse)
    println(word.toLowerCase())
    println(word.replace("He", "E"))
    println(word.replace("!", ", ").concat(addedFrase))
  }

  def monthlySalary(annualIncome: Double, premium: Double, lunches: Double, taxIncome: Double): Unit = {
    //вычитаем обеды, т.к. они не облагаются налогом
    var bezPitaniya = annualIncome - lunches * 12
    //убираем премию
    var bezPremii = bezPitaniya - (bezPitaniya * (1 - premium / 100))
    //делим на год
    var everyMonth = bezPremii / 12
    //вычитаем налог
    var clearMonthlySalary = everyMonth * (1 - taxIncome / 100)
    //println("Размер ежемесячного оклада составляет: " + clearMonthlySalary.round)
    println(f"Размер ежемесячного оклада составляет:  $clearMonthlySalary%.2f")
  }

  def salaryDeviation(salaries: List[Double]) = {
    val avrSalary: Double = salaries.sum / salaries.length.toDouble
    println(f"Средний оклад составляет:  $avrSalary%.2f")
    val avrDevSalary = salaries.map(salary => (salary / avrSalary - 1) * 100).mkString(", ")
    println("Отклонение среднего значения оклада на уровне отдела составляет: " + avrDevSalary)

  }

  def correctionBonus(salaries: List[Double], correction: List[Double]) = {
    var resultSalary = (salaries, correction).zipped.map(_ * _)
    println("Зарплата после премиальной корректировки составляет: " + resultSalary.mkString(", "))
  }

  def sortedWithNewEmpl(salaries: List[Double], addSalaries: List[Double]) = {
    println("Отсортированный список зарплат включая новых сотрудников: " + (salaries ::: addSalaries).sorted.mkString(", "))
  }

  def addNewEmployeeWithSorted(salaries: List[Double], addSalaries: List[Double], newEmpl: Double) = {
    val sortedAllEmpl = (salaries ::: addSalaries).sorted
    val indexNewEmpl: Int = sortedAllEmpl.indexWhere(sal => sal >= newEmpl)
    val resultAllEmpl = sortedAllEmpl.dropRight(indexNewEmpl - 1) ++ List(newEmpl) ++ sortedAllEmpl.drop(indexNewEmpl)
    var i: Int = 0
    //println("Список сотрудников с новым добавленным после сортировки: " + resultAllEmpl.mkString(", "))
    println("Список сотрудников с новым добавленным после сортировки: ")
    while (i < resultAllEmpl.length) {
      println(i + ": " + resultAllEmpl(i))
      i += 1
    }
  }

  def numberOfEmployeeOfMidle(salaries: List[Double], addSalaries: List[Double], newEmpl: Double, rangeMin: Double, rangeMax: Double) = {
    val sortedAllEmpl = (salaries ::: addSalaries).sorted
    val indexNewEmpl: Int = sortedAllEmpl.indexWhere(sal => sal >= newEmpl)
    val resultAllEmpl = sortedAllEmpl.dropRight(indexNewEmpl - 1) ++ List(newEmpl) ++ sortedAllEmpl.drop(indexNewEmpl)
    var i: Int = 0
    println("Список сотрудников из категории: ")
    while (i < resultAllEmpl.length) {
      if (resultAllEmpl(i) > rangeMin && resultAllEmpl(i) < rangeMax) println(i + ": " + resultAllEmpl(i))
      i += 1
    }
  }

  def indexingSalary(salaries: List[Double], addSalaries: List[Double], newEmpl: Double, percentIndexing: Double) = {
    val sortedAllEmpl = (salaries ::: addSalaries).sorted
    val indexNewEmpl: Int = sortedAllEmpl.indexWhere(sal => sal >= newEmpl)
    val resultAllEmpl = sortedAllEmpl.dropRight(indexNewEmpl - 1) ++ List(newEmpl) ++ sortedAllEmpl.drop(indexNewEmpl)
    val resultAfterIndexing = resultAllEmpl.map(sal => sal * (1 + percentIndexing / 100))
    var i: Int = 0
    println("Список сотрудников после индексирования: ")
    while (i < resultAfterIndexing.length) {
      println(i + ": " + resultAfterIndexing(i))
      i += 1
    }
  }
}
