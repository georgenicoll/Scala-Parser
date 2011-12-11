package com.monkeynuthead.mdx.parser.expressions

import org.scalatest.Spec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SetValueExpressions extends Spec {

	describe("Set Value Expressions") {

		info("http://msdn.microsoft.com/en-us/library/windows/desktop/ms711544(v=VS.85).aspx")

		describe("can parse") {

			it("set of all members <dimension>.MEMBERS") {
				pending
			}

			it("all of the members in a hierarchy <hierarchy>.MEMBERS") {
				pending
			}

			it("all members at a given level in a dimension <level>.MEMBERS") {
				pending
			}

			it("all children of a member <member>.CHILDREN") {
				pending
			}

			it("ancestor of a member that is a distance away ANCESTOR(<member>, <distance>)") {
				pending
			}

			it("ancestors of a member at the specified level ANCESTORS(<member>, <level>)") {
				pending
			}

			it("ancestors of a member at a specified distance ANCESTORS(<member>, <distance>)") {
				pending
			}

			it("cross join of the input sets CROSSJOIN(<set1>, <set2>)") {
				pending
			}

			it("descendents of a member at a given level DESCENDANTS(<member>, <level> [, <desc_flags>])") {
				pending
			}

			it("descendents of a member at a given distance away DESCENDANTS(<member>, <distance> [, <desc_flags>])") {
				pending
			}

			it("distinct tuple from a set DISTINCT(<set>)") {
				pending
			}

			it("except - the difference between 2 sets EXCEPT(<set1>, <set2> [,[ALL]])") {
				pending
			}

			it("extract EXTRACT(<set>, <dimension>[, <dimension>...])") {
				pending
			}
			
			it("filterFILTER(<set>, <search_condition>)") {
				pending
			}
			
			it("generate GENERATE(<set1>, <set2> [,[ALL]])") {
				pending
			}
			
			it("heirarchize HIERARCHIZE(<set>)") {
				pending
			}
			
			it("intesect INTERSECT(<set1>, <set2> [,[ALL]])") {
				pending
			}
			
			it("order ORDER(<set>, {<string_value_expression> | <numeric_value_expression>} [, ASC | DESC | BASC | BDESC])") {
				pending
			}
			
			it("topcount TOPCOUNT(<set>, <index> [, <numeric_value_expression>])") {
				pending
			}

			it("toppercent TOPPERCENT(<set>, <percentage>, <numeric_value_expression>)") {
				pending
			}

      it("topsum TOPSUM(<set>, <value>, <numeric_value_expression>)") {
				pending
			}

      it("union UNION(<set1>, <set2> [, [ALL]])") {
				pending
			}

		}

	}

}