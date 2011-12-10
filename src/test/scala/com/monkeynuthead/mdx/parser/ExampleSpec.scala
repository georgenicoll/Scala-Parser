package com.monkeynuthead.mdx.parser

import org.scalatest.Spec
import org.scalatest.BeforeAndAfter
import scala.collection.mutable.Stack

class ExampleSpec extends Spec with BeforeAndAfter {
  
  var stack: Stack[Int] = _
  
  before {
    stack = new Stack[Int]
  }

  describe("A Stack") {
    
    ignore("just fails") {
      fail()
    }
    
    it("should pop values in last-in-first-out-order") {
      stack.push(1, 2)
      expect(2) { stack.pop() }
      expect(1) { stack.pop() }
    }
    
    it("should throw NoSuchElementException if an empty stack is popped") {
      intercept[NoSuchElementException] {
        stack.pop()
      }
    }
    
  }
  
}