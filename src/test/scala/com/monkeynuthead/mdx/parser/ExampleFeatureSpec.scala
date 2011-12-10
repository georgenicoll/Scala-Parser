package com.monkeynuthead.mdx.parser

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.Stack

class ExampleFeatureSpec extends FeatureSpec with GivenWhenThen with BeforeAndAfter {
  
  var stack: Stack[Int] = _
  
  before {
    stack = new Stack[Int]
  }
  
  feature("The user can pop an element off the top of the stack") {
    
    info("As a programmer")
    info("I want to be able to pop items off the stack")
    info("So that I get them in last-in-first-out order")
    
    scenario("pop is invoked on a non-empty stack") { 
      given("a non-empty stack")
      stack.push(1, 2)
      val oldSize = stack.size
      
      when("pop is invoked on the stack")
      val result = stack.pop()
      
      then("the most recently pushed element should be returned")
      assert(result === 2)
      
      and("the stack should have one less item than before")
      assert(stack.size === oldSize - 1)
    }
    
    scenario("pop is invoked on an empty stack") {
      given("an empty stack")
      
      when("when pop is invoked on the stack")
      then("NoSuchElementException should be thrown")
      intercept[NoSuchElementException] {
        stack.pop()
      }
      
      and("the stack should still be empty")
      assert(stack.isEmpty)
    }
    
  }

}