//Scala 2.12.3
//'Dcoder' object is the entry point
//for your code.
//Don't declare a package.
import scala.io.StdIn
import scala.collection.mutable.ListBuffer;
//програма(телефонная) на которой я писал код(for android)
  //object Dcoder extends App {
    
// для компютера  убрать тут коментариц и в конце(for pk)
object Dcoder {
def main(args: Array[String]): Unit = 
  {
 var birja = new Birja();
  
 var stud1 = new Student("Dasha", "Ivanova",18,
 new Adress("Ukraine", "Lviv","12a"),
 new Wallet(0, 5000), 241);
 var stud2 = new Student("Oleksandr", "Maidanika",19,
 new Adress("Ukraine", "Lviv","1a"),
 new Wallet(0, 5000), 241);
 var stud3 = new Student("Kirill", "Scholniy",19,
 new Adress("Ukraine", "Odessa","75"),
 new Wallet(0, 5000), 241);
  
  
 var teach1 = new Teacher("Anton", "Jakobs",28,
 new Adress("Ukraine", "Odessa","1b"),
 new Wallet(10000, 1000),new Course(1000));
 
 
 teach1.AddToList(stud1);
 teach1.AddToList(stud2);
 teach1.AddToList(stud3);
 
  var sim1 = new Simulation();

  sim1.Add(teach1);
  sim1.SimulationOfYears(birja)


 
}



class Simulation (){
 var _list_teacher = ListBuffer[Teacher]();
 var counterofmounth:Int = 10
 
 
 def Add(human: Teacher) :Unit ={
     _list_teacher+= human
  }  
  
  def SimulationOfYears(birja: Birja){
    for(j <- 0 until counterofmounth){
      for(i <- 0 until _list_teacher.length){
        
        _list_teacher(i).Givemarks();
        for(k <- 0 until _list_teacher(i)._list_student.length){
        _list_teacher(i)._list_student(k).OplataOrStep(_list_teacher(i).course.Course_Cost, birja)
        
        
           }
           _list_teacher(i).PrintInformationAboutStudent()
        }
        
        birja.course_change();
        }
     }
  
}





class Adress (var country: String, var city: String, var house: String){
  override def toString(): String =s"Country: $country, City: $city, House: $house";
}

class Birja(){
  
 private var tokens_count: Int = scala.util.Random.nextInt(50000)+ 10000;
 private var fiat_money_count: Int = scala.util.Random.nextInt(30000)+ 10000;
 private var tokens_buy_cost : Double = (scala.util.Random.nextDouble()+ 0.9);
 private var tokens_sell_cost : Double = (scala.util.Random.nextDouble()+ 0.7);

def Tokens_Count: Int = tokens_count
def Fiat_Money_Count: Int = fiat_money_count
def Tokens_Buy_Cost: Double = tokens_buy_cost
def Tokens_Sell_Cost: Double = tokens_sell_cost

def ShowCourse(){
  println("стоимость покупки одного токена = " + tokens_buy_cost) 
  println("стоимость продажи одного токена = " + tokens_sell_cost)
}
def SellTokens(count:Int):Int=
{
  var fiat_money_return:Int = 0
  if(fiat_money_count < (count*tokens_sell_cost).toInt){
    println("На бирже недостаточно Денег. \n На данный момент вы можете продать на " + (fiat_money_count))
    tokens_count = tokens_count + (fiat_money_count *tokens_sell_cost).toInt;
    fiat_money_count = fiat_money_count - fiat_money_count
    fiat_money_return = fiat_money_count
  }else{
    tokens_count += count;
    fiat_money_count = fiat_money_count - (count * tokens_sell_cost).toInt
     fiat_money_return = (count * tokens_sell_cost).toInt
  }
  return fiat_money_return.toInt
  }


def BuyTokens(count:Int):Int=
{
  var token_return:Int = 0
if(tokens_count < count){
    println("На бирже недостаточно токенов. \n На данный момент вы можете купить " + tokens_count)
    fiat_money_count = fiat_money_count + (tokens_count * tokens_buy_cost).toInt
    tokens_count -= tokens_count;
    return tokens_count
  }else{
    fiat_money_count = fiat_money_count + (count * tokens_buy_cost).toInt
     tokens_count -= count;
    token_return = count
  }  
return token_return.toInt
}


def course_change(){
  this.tokens_buy_cost =(scala.util.Random.nextDouble()+ 0.9);
  this.tokens_sell_cost =(scala.util.Random.nextDouble()+ 0.7);
}
}
class Wallet (var tokens: Int , var fiatmoney: Int){
  private var _tokens: Int = tokens;
  private var _fiatmoney: Int = fiatmoney;
  
  
 override def toString(): String =s"Tokens: $tokens, Fiat Money: $fiatmoney";
 def Tokens: Int = _tokens;
 def Fiatmoney: Int = _fiatmoney;
}


class Human (var name: String, var surname: String, var age: Int, var adress: Adress ){
  
  private var _name:String = name;
  private var _surname:String = surname;
  private var _age:Int = age;
  private var _adress: Adress = adress;
  
  def Name: String = _name;
  def Surame: String = _surname;
  def Age: Int = _age;
  def Adress: Adress = _adress;
 
 override def toString(): String =s"Name: $name \nSurname: $surname \nAge: $age \nAdress: $adress\n";
}

class Course(var course_cost: Int){
  private var _course_cost: Int = course_cost;
  
  
  def Course_Cost: Int = _course_cost
  override def toString(): String =s"Course cost: $course_cost\n "
}

class Student( name: String, surname: String, age: Int, adress: Adress, var wallet: Wallet, var group: Int )extends Human(name, surname, age, adress ){  
  private var _wallet: Wallet = wallet;
  private var _group: Int  = group;
  private var _marks: Int = 0;
  
  def Wallet: Wallet = _wallet;
  def Group: Int = _group;
  def Marks: Int = _marks
  def setMarks(value:Int){
    _marks = value
  }
  
  def OplataOrStep(cost:Int, birja: Birja)
  {
    var cst: Int = cost
      /* 
      1: -10%
      2: -5%
      3: 0%
      4: 5%
      5: 10%
      */
       if(_marks == 1){
        cst /= 10
          if (cst > _wallet.tokens){
            Moneytotoken(cst - _wallet.tokens, birja)
            _wallet.tokens -= cst
          }else{
        _wallet.tokens -= cst}
       }else if (_marks == 2){
       cst = (cost / 100)*5
        if (cst > _wallet.tokens){
            Moneytotoken(cst - _wallet.tokens, birja)
            _wallet.tokens -= cst
          }else{
        _wallet.tokens -= cst}
       }else if (_marks == 3){
        _wallet.tokens -= 0
       }else if (_marks == 4){
        cst = (cost / 100)*5
        _wallet.tokens += cst
       }else if (_marks == 5){
        cst /= 10
        _wallet.tokens += cst
       }
  }
  
  def Moneytotoken(money:Int, birja: Birja){
    birja.ShowCourse();
    var temp_money: Int = _wallet.fiatmoney 
    var temp_tokens:Int = _wallet.tokens
    var temp_tokens_buycost: Double = birja.Tokens_Buy_Cost
    var temp_tokens_sellcost: Double = birja.Tokens_Sell_Cost
    var temp1 : Int = birja.BuyTokens(money)
    _wallet.tokens = _wallet.tokens + temp1
    _wallet.fiatmoney -= (temp1 * birja.Tokens_Buy_Cost).toInt
  }
  
  override def toString(): String = super.toString +
    "Wallet: " + wallet.toString() + "\n"+
    "Group: " + group.toString() + "\n"+
    "Marks: " + _marks.toString() + "\n"
  
  
  def BuySellTokens(birja: Birja){
    print( "\n"+this.name +" "+ this.surname +" У вас на балансе " + _wallet.tokens +" токенов \n")
    println(" У вас на балансе " + _wallet.fiatmoney +" денег \n")
    birja.ShowCourse();
    var temp_money: Int = _wallet.fiatmoney 
    var temp_tokens:Int = _wallet.tokens
    var temp_tokens_buycost: Double = birja.Tokens_Buy_Cost
    var temp_tokens_sellcost: Double = birja.Tokens_Sell_Cost
    println("ведите: 1 купить токены \nведите: 2 продать токены ")
    var choice: Int = scala.io.StdIn.readInt();
    if(choice==1){
    println("Вы можете купить "+ (temp_money/temp_tokens_buycost) +" токенов \n Ведите сколько токенов хотите купить")
    var tokencount : Int  = scala.io.StdIn.readInt();
     if(_wallet.fiatmoney < (tokencount *birja.Tokens_Buy_Cost).toInt){ println(" У вас не достаточно денег")}
    else{
      var temp1 : Int = birja.BuyTokens(tokencount)
      _wallet.tokens = _wallet.tokens + temp1
       _wallet.fiatmoney -= (temp1 * birja.Tokens_Buy_Cost).toInt
      
    }}
    if(choice == 2){
      println("Вы и получить  "+ (temp_tokens*temp_tokens_sellcost) +"денег \n Ведите сколько токенов хотите продать")
      var tokencount : Int  = scala.io.StdIn.readInt();
      if(_wallet.tokens < tokencount){ println(" У вас не достаточно токенов")
    }else{
      var temp1 : Int = birja.BuyTokens(tokencount)
     _wallet.fiatmoney = _wallet.fiatmoney + temp1
     _wallet.tokens = _wallet.tokens -(temp1 * birja.Tokens_Buy_Cost).toInt
    }
    
  }

  }
  }
  


class Teacher( name: String, surname: String, age: Int, adress: Adress, var wallet:Wallet, var course : Course )extends Human(name, surname, age, adress ){
  private var _wallet: Wallet = wallet;
  private var _course: Course = course;
  var _list_student = ListBuffer[Student]();
  
  def Wallet: Wallet = _wallet;
  def Course: Course = _course;
  
  override def toString(): String = super.toString +
    "Wallet: " + wallet.toString() + "\n"+
    "Course: " + course.toString() + "\n"
  
  
  def AddToList(human: Student) :Unit ={
     _list_student += human
  }
  
  def PrintInformationAboutStudent():Unit ={
    for(i <- 0 until _list_student.length){
      println("\n"+this._list_student(i).toString)}
  }
  
  def Givemarks(){
    for(i <- 0 until _list_student.length){
      var temp: Int = scala.util.Random.nextInt(4)+ 1;
      _list_student(i).setMarks(temp); }
  }
  
  def BuySellTokens(birja: Birja){
    print( "\n"+this.name +" "+ this.surname +" У вас на балансе " + _wallet.tokens +" токенов \n")
    println(" У вас на балансе " + _wallet.fiatmoney +" денег \n")
    birja.ShowCourse();
    var temp_money: Int = _wallet.fiatmoney 
    var temp_tokens:Int = _wallet.tokens
    var temp_tokens_buycost: Double = birja.Tokens_Buy_Cost
    var temp_tokens_sellcost: Double = birja.Tokens_Sell_Cost
    println("ведите: 1 купить токены \nведите: 2 продать токены ")
    var choice: Int = scala.io.StdIn.readInt();
    if(choice==1){
    println("Вы можете купить "+ (temp_money/temp_tokens_buycost) +" токенов \n Ведите сколько токенов хотите купить")
    var tokencount : Int  = scala.io.StdIn.readInt();
     if(_wallet.fiatmoney < (tokencount *birja.Tokens_Buy_Cost).toInt){ println(" У вас не достаточно денег")}
    else{
      var temp1 : Int = birja.BuyTokens(tokencount)
      _wallet.tokens = _wallet.tokens + temp1
       _wallet.fiatmoney -= (temp1 * birja.Tokens_Buy_Cost).toInt
      
    }}
    if(choice == 2){
      println("Вы и получить  "+ (temp_tokens*temp_tokens_sellcost) +"денег \n Ведите сколько токенов хотите продать")
      var tokencount : Int  = scala.io.StdIn.readInt();
      if(_wallet.tokens < tokencount){ println(" У вас не достаточно токенов")
    }else{
      var temp1 : Int = birja.BuyTokens(tokencount)
     _wallet.fiatmoney = _wallet.fiatmoney + temp1
     _wallet.tokens = _wallet.tokens -(temp1 * birja.Tokens_Buy_Cost).toInt
    }
    
  }

  }
 }
}

