

/**
 * @author nhphung
 */
package bkool

import java.io.{PrintWriter,File}
import java.util.concurrent.{Executors,TimeUnit,TimeoutException}
import org.antlr.v4.runtime.ANTLRFileStream
//import bkool.utils._
import bkool.parser._

trait Timed {
  def timeoutAfter(timeout: Long)(codeToTest: => Unit): Unit = {
    val executor = Executors.newSingleThreadExecutor
    val future = executor.submit(new Runnable {
      def run = codeToTest
    })

    try {
      future.get(timeout, TimeUnit.MILLISECONDS)
    }
    finally {
      executor.shutdown()
    }
  }
}

object Main extends  Timed {
  
  val sepa = "//" // dung cho linux
  
  def main(args: Array[String]): Unit = {
    if (args.length > 0) {
      val option = args(0).drop(1)
      
      val startphase1 = 1
      val endphase1 = 3
      val indirphase1 = "lexertestcases"
      val outdirphase1 = "lexersolutions"
      
      val startphase2 = 1
      val endphase2 = 3
      val indirphase2 = "recogtestcases"
      val outdirphase2 = "recogsolutions"
      
      val startphase3 = 1
      val endphase3 = 3
      val indirphase3 = "asttestcases"
      val outdirphase3 = "astsolutions"
      
      
      
      option match {
        
        case "testphase1" => runTest(option,startphase1,endphase1,indirphase1,outdirphase1)
        case "testphase2" => {
            runTest("testphase1",startphase1,endphase1,indirphase1,outdirphase1)
            runTest(option,startphase2,endphase2,indirphase2,outdirphase2)
        }
        case "testphase3" => {
            runTest("testphase1",startphase1,endphase1,indirphase1,outdirphase1)
            runTest("testphase2",startphase2,endphase2,indirphase2,outdirphase2)
            runTest(option,startphase3,endphase3,indirphase3,outdirphase3)
        }
        case _ => throw new ClassCastException
      }
    }
    else  println("Usage: scala Main -option ")
  }
  
  def runTest(opt:String,start:Int,end:Int,indir:String,outdir:String) = {
    
    for (i <- start to end) {
      
      println("Test "+i)
      
      
      val source = new ANTLRFileStream(s"$indir$sepa$i.txt")
      val dest = new PrintWriter(new File(s"$outdir$sepa$i.txt"))
      
      try 
      {
        timeoutAfter(1000)  
        {
            opt match {
              case "testphase1" => TestLexer.test(source,dest) 
              case "testphase2" => TestParser.test(source,dest)              
              case "testphase3"  => TestAst.test(source,dest)
              case _ => throw new ClassCastException
          }
        }
      } 
      catch 
      {
        case te: TimeoutException => dest.println("Test runs timeout")
        //case e : Exception => dest.println(e)
      } 
      finally 
      {
        //source.close()
        dest.close()

      }
    }
  } 
}