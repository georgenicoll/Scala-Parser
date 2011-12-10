package com.monkeynuthead.mdx.parser

import scala.util.parsing.combinator._

class MDX extends JavaTokenParsers {
  
  import MDX._
  //See http://msdn.microsoft.com/en-us/library/windows/desktop/ms721196(v=vs.85).aspx 

  /** MDX Elements */
  
  //<identifier> ::= <regular_identifier> | <delimited_identifier>
  def identifier: Parser[Any] = regular_identifier | delimited_identifier
  
  //<regular_identifier> ::= <alpha_char> [{<alpha_char> | <digit>
  //                                     | <underscore>}...]
  def regular_identifier: Parser[Any] = alpha_char~opt(rep(alpha_char | digit | underscore))

  //<delimited_identifier> ::=
  //    <start_delimiter>{<double_end_delimiter> | <nondelimit_end_symbol>}
  //    [{<double_end_delimiter> | <nondelimit_end_symbol> }...]
  //    <end_delimiter>
  def delimited_identifier: Parser[Any] = start_delimiter~rep(double_end_delimiter | nondelimit_end_symbol)~
  										  opt(rep(double_end_delimiter | nondelimit_end_symbol))~
  										  end_delimiter

  //<start_delimiter> ::= <open_bracket>
  def start_delimiter: Parser[Any] = open_bracket

  //<end_delimiter> ::= <close_bracket>
  def end_delimiter: Parser[Any] = close_bracket

  //<double_end_delimiter> ::= <end_delimiter> <end_delimiter>
  def double_end_delimiter: Parser[Any] = end_delimiter~end_delimiter

  //<nondelimit_end_symbol> ::= !! Any character except <end_delimiter>
  def nondelimit_end_symbol: Parser[Any] = not(end_delimiter)

  //<cube_name> ::= [ [ [ <data_source>.] <catalog_name>.] [<schema_name>.]
  //                <identifier>
  def cube_name: Parser[Any] = opt(opt(opt(data_source~".")~catalog_name~".")~schema_name~".")~identifier

  //<data_source> ::= <identifier>
  def data_source: Parser[Any] = identifier

  //<catalog_name> ::= <identifier>
  def catalog_name: Parser[Any] = identifier

  //<schema_name> ::= <identifier>
  def schema_name: Parser[Any] = identifier

  //<dim_hier> ::= [<cube_name>.]<dimension_name>
  //             | [[<cube_name>.]< dimension_name>.]<hierarchy_name>
  def dim_hier: Parser[Any] = opt(cube_name~".")~dimension_name |
                              opt(opt(cube_name~".")~dimension_name~".")~hierarchy_name

  
                                   
  //<dimension_name> ::= <identifier>
  //                   | <member>.DIMENSION
  //                   | <level>.DIMENSION
  //                   | <hierarchy>.DIMENSION
  def dimension_name: Parser[Any] = identifier |
  									member~"."~DIMENSION |
  									level~"."~DIMENSION |
  									hierarchy~"."~DIMENSION

  //<dimension> ::= <dimension_name>
  def dimension: Parser[Any] = dimension_name

  //<hierarchy> ::= <hierarchy_name>
  def hierarchy: Parser[Any] = hierarchy_name

  //<hierarchy_name> ::= <identifier>
  //                   | < member>.HIERARCHY
  //                   | <level>.HIERARCHY
  def hierarchy_name: Parser[Any] = identifier |
                                    member~"."~HIERARCHY |
                                    level~"."~HIERARCHY

  //<level> ::= [<dim_hier>.]< identifier>
  //          | <dim_hier>.LEVELS(<index>)
  //          | <member>.LEVEL
  def level: Parser[Any] = opt(dim_hier)~identifier |
                           dim_hier~"."~LEVELS~"("~index~")" |
                           member~"."~LEVELS
                                		
                                
  //<member> ::= [<level>.]<identifier>
  //           | <dim_hier>.<identifier>
  //           | <member>.<identifier>
  //           | <member_value_expression>                                
  def member: Parser[Any] = opt(level~".")~identifier |
                            dim_hier~"."~identifier |
                            member~"."~identifier |
                            member_value_expression
                            
  //<property> ::= <mandatory_property> | <user_defined_property>
  def property: Parser[Any] = mandatory_property | user_defined_property

  //<mandatory_property> ::= CATALOG_NAME
  //                       | SCHEMA_NAME
  //                       | CUBE_NAME
  //                       | DIMENSION_UNIQUE_NAME
  //                       | HIERARCHY_UNIQUE_NAME
  //                       | LEVEL_UNIQUE_NAME
  //                       | LEVEL_NUMBER
  //                       | MEMBER_UNIQUE_NAME
  //                       | MEMBER_NAME
  //                       | MEMBER_TYPE
  //                       | MEMBER_GUID
  //                       | MEMBER_CAPTION
  //                       | MEMBER_ORDINAL
  //                       | CHILDREN_CARDINALITY
  //                       | PARENT_LEVEL
  //                       | PARENT_UNIQUE_NAME
  //                       | PARENT_COUNT
  //                       | DESCRIPTION
  def mandatory_property[Any] = CATALOG_NAME |
                                SCHEMA_NAME |
                                CUBE_NAME |
                                DIMENSION_UNIQUE_NAME |
                                HIERARCHY_UNIQUE_NAME |
                                LEVEL_UNIQUE_NAME |
                                LEVEL_NUMBER |
                                MEMBER_UNIQUE_NAME |
                                MEMBER_NAME |
                                MEMBER_TYPE |
                                MEMBER_GUID |
                                MEMBER_CAPTION |
                                MEMBER_ORDINAL |
                                CHILDREN_CARDINALITY |
                                PARENT_LEVEL |
                                PARENT_UNIQUE_NAME |
                                PARENT_COUNT |
                                DESCRIPTION

  //<user_defined_property> ::= <dim_hier>.<identifier>
  //                          | <level>.<identifier>
  //                          | <member>.<identifier>
  def user_defined_property: Parser[Any] = dim_hier~"."~identifier |
                                           level~"."~identifier |
                                           member~"."~identifier
                                
  //<tuple> ::= <member> 
  //          | (<member> [, <member>...])
  //          | <tuple_value_expression>
  def tuple: Parser[Any] = member |
                           repsep(member, ",") |
                           tuple_value_expression
                           
  //<set> ::= <member>:<member>
  //          | <set_value_expression>
  //          | <open_brace>[<set>|<tuple> [, <set>|<tuple>...]]<close_brace>
  //          | (<set>)                           
  def set: Parser[Any] = member~":"~member |
                         set_value_expression |
                         open_brace~repsep(set | tuple, ",")~close_brace |
                         "("~set~")"
    
  //<open_brace> ::= {
  def open_brace: Parser[Any] = "{"

  //<close_brace> ::= }
  def close_brace: Parser[Any] = "}"

  //<open_bracket> ::= [
  def open_bracket: Parser[Any] = "["

  //<close_bracket> ::= ]
  def close_bracket: Parser[Any] = "]"

  //<underscore> ::= _
  def underscore: Parser[Any] = "_"

  //<alpha_char> ::= a | b | c | ...| z | A | B | C | ... | Z
  def alpha_char: Parser[Any] = """[a-zA-Z]""".r 

  //<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
  def digit: Parser[Any] = """[0..9]""".r

  /** MDX Statement Definitions */
  
  //<value_expression> ::= <numeric_value_expression>
  //                     | <string_value_expression>
  def value_expression: Parser[Any] = numeric_value_expression |
                                      string_value_expression
                                      
  //<numeric_value_expression> ::= <term>
  //                            | <numeric_value_expression> {<plus> | <minus>} <term>
  def numeric_value_expression: Parser[Any] = term |
                                              numeric_value_expression~(plus | minus)~term

  //<term> ::= <factor> | <term> {<asterisk> | <solidus>} <factor>
  def term: Parser[Any] = factor | term~(asterisk | solidus)~factor

  //<factor> ::= [<sign>] <numeric_primary>
  def factor: Parser[Any] = opt(sign)~numeric_primary

  //<sign> ::= + | -
  def sign: Parser[String] = plus | minus

  //<plus> ::= +
  def plus: Parser[String] = "+"

  //<minus> ::= -
  def minus: Parser[String] = "-"

  //<asterisk>::= *
  def asterisk: Parser[String] = "*"

  //<solidus> ::= /
  def solidus: Parser[String] = "/"

  //<numeric_primary> ::= <value_expression_primary>
  //                  | <numeric_value_function>
  def numeric_primary: Parser[Any] = value_expression_primary | numeric_value_function
  
  //<value_expression_primary> ::= <unsigned_numeric_literal>
  //                             | (<value_expression>)
  //                             |  <character_string_literal>
  //                             | [<cube_name>.]<tuple>[.VALUE]
  //                             |  <property>[.VALUE]
  //                             |  <conditional_expression>
  def value_expression_primary: Parser[Any] = unsigned_numeric_literal |
                                              "("~value_expression~")" |
                                              character_string_literal |
                                              opt(cube_name~".")~tuple~opt("."~VALUE) |
                                              property~opt("."~VALUE) |
                                              conditional_expression

  //<conditional_expression> ::= <if_expression> | <case_expression>
  def conditional_expression: Parser[Any] = if_expression | case_expression
  
  //<case_expression> ::= <simple_case> | <searched_case> | <coalesce_empty>
  def case_expression: Parser[Any] = simple_case | searched_case | coalesce_empty

  //<coalesce_empty> ::= COALESCEEMPTY (<value_expression> ,
  //                                     <value_expression>
  //                                    [, <value_expression> ]...)
  def coalesce_empty: Parser[Any] = COALESCEEMPTY~"("~repsep(value_expression, ",")~")"

  //<unsigned_numeric_literal> ::= <exact_numeric_literal>
  //                             | <approximate_numeric_literal>
  def unsigned_numeric_literal: Parser[Any] = exact_numeric_literal | approximate_numeric_literal

  //<exact_numeric_literal> ::= <unsigned_integer>[.<unsigned_integer>]
  //                          | <unsigned_integer>.
  //                          | .<unsigned_integer>
  def exact_numeric_literal: Parser[Any] = unsigned_integer~opt("."~unsigned_integer) |
                                           unsigned_integer~"." |
                                           "."~unsigned_integer

  //<unsigned_integer> ::= {<digit>}...
  def unsigned_integer: Parser[Any] = rep("""\d+""".r)

  //<approximate_numeric_literal> ::= <mantissa>E<exponent>
  def approximate_numeric_literal: Parser[Any] = mantissa~E~exponent

  //<mantissa> ::= < exact_numeric_literal>
  def mantissa: Parser[Any] = exact_numeric_literal

  //<exponent> ::= [<sign>]<unsigned_integer>
  def exponent: Parser[Any] = opt(sign)~unsigned_integer

  //<string_value_expression> ::= <value_expression_primary>
  //                            | <string_value_expression>
  //                              <concatenation_operator>
  //                              <value_expression_primary>
  def string_value_expression: Parser[Any] = value_expression_primary |
                                             string_value_expression~concatenation_operator~value_expression_primary
                                             
  //<character_string_literal>::= <quote>[<character_representation>...]
  //                              <quote>
  def character_string_literal: Parser[Any] = quote~rep(character_representation)~quote

  //<character_representation> ::= <nonquote_character> | <quote_symbol>
  def character_representation: Parser[Any] = nonquote_character | quote_symbol

  //<nonquote_character> ::= !!
  //                    Any character in the character set other than <quote>
  //FIXME:  Check this
  def nonquote_character: Parser[Any] = """!'""".r 

  //<quote_symbol> ::= <quote> <quote>
  def quote_symbol: Parser[Any] = quote~quote

  //<quote>  ::= '
  def quote: Parser[Any] = "'"

  //<concatenation_operator> ::= ||       
  def concatenation_operator: Parser[String] = "||"
  
  /** MDX Set Value Expressions */
                                      
  //<index> ::= <numeric_value_expression>
  def index: Parser[Any] = numeric_value_expression

  //<percentage> ::= <numeric_value_expression>
  def percentage: Parser[Any] = numeric_value_expression

  //<set_value_expression> ::= <dim_hier>.MEMBERS
  //                         | <level>.MEMBERS
  //                         | <member>.CHILDREN
  //                         | BOTTOMCOUNT(<set>, <index>
  //                           [, <numeric_value_expression>])
  //                         | BOTTOMPERCENT(<set>, <percentage>,
  //                           <numeric_value_expression>)
  //                         | BOTTOMSUM(<set>, <numeric_value_expression>,
  //                           <numeric_value_expression>)
  //                         | CROSSJOIN(<set>, <set>)
  //                         | DESCENDANTS(<member>, <level> [,<desc_flags>]) //absence of desc_flags use self
  def set_value_expression: Parser[Any] = dim_hier~"."~MEMBERS |
                                          level~"."~MEMBERS |
                                          member~"."~CHILDREN |
                                          BOTTOMCOUNT~"("~set~","~index~opt(","~numeric_value_expression)~")" |
                                          BOTTOMSUM~"("~set~","~numeric_value_expression~","~numeric_value_expression~")" |
                                          CROSSJOIN~"("~set~","~set~")" |
                                          DESCENDANTS~"("~member~","~level~opt(","~desc_flags)~")" |
  //                         | DISTINCT(<set>)
  //                         | DRILLDOWNLEVEL(<set> [, <level>])
  //                         | DRILLDOWNLEVELBOTTOM(<set>, <index>
  //                           [,[<level>], <numeric_value_expression>])
  //                         | DRILLDOWNLEVELTOP(<set>, <index>[, [<level>]
  //                           , <numeric_value_expression>])
  //                         | DRILLDOWNMEMBER(<set>, <set>[, RECURSIVE])
  //                         | DRILLDOWNMEMBERBOTTOM(<set>, <set>, <index>
  //                           [, <numeric_value_expression>], RECURSIVE])
  //                         | DRILLDOWNMEMBERTOP(<set>, <set>, <index>
  //                           [, [<numeric_value_expression>], RECURSIVE])
  //                         | DRILLUPLEVEL(<set>[, <level>])
  //                         | DRILLUPMEMBER(<set>, <set>)
  //                         | EXCEPT(<set>, <set> [, [ALL]])
  //                         | EXTRACT(<set>, <dim_hier>[, <dim_hier>...])
  //                         | FILTER(<set>, <search_condition>)
  //                         | GENERATE(<set>, <set> [, [ALL]])
  //                         | HIERARCHIZE(<set>)
  //                         | INTERSECT(<set>, <set> [, [ALL]])
  //                         | LASTPERIODS(<index> [, <member>])
  //                         | MTD([<member>])
  //                         | ORDER(<set>, <value_expression>
  //                           [, ASC | DESC | BASC | BDESC])
                                          DISTINCT~"("~set~")" |
                                          DRILLDOWNLEVEL~"("~set~opt(","~level)~")" |
                                          DRILLDOWNLEVELBOTTOM~"("~set~","~index~
                                            opt(opt(","~level)~","~numeric_value_expression)~")" |
                                          DRILLDOWNLEVELTOP~"("~set~","~index~
                                            opt(opt(","~level)~","~numeric_value_expression)~")" |
                                          DRILLDOWNMEMBER~"("~set~","~set~opt(","~RECURSIVE)~")" |
                                          DRILLDOWNMEMBERBOTTOM~"("~set~","~set~","~index~
                                            opt(opt(","~numeric_value_expression)~","~RECURSIVE)~")" |
                                          DRILLDOWNMEMBERTOP~"("~set~","~set~","~index~
                                            opt(opt(","~numeric_value_expression)~","~RECURSIVE)~")" |
                                          DRILLUPLEVEL~"("~set~opt(","~level)~")" |
                                          DRILLUPMEMBER~"("~set~","~set~")" |
                                          EXCEPT~"("~set~","~set~opt(","~ALL)~")" |
                                          EXTRACT~"("~set~","~dim_hier~rep(","~dim_hier)~")" |
                                          FILTER~"("~set~","~search_condition~")" |
                                          GENERATE~"("~set~","~set~opt(","~ALL)~")" |
                                          HIERARCHIZE~"("~set~")" |
                                          INTERSECT~"("~set~","~set~opt(","~ALL)~")" |
                                          LASTPERIODS~"("~index~opt(","~member)~")" |
                                          MTD~"("~member~")" |
                                          ORDER~"("~set~","~value_expression~
                                            opt(ASC | DESC | BASC | BDESC)~")" | //ASC is the default
  //                         | PERIODSTODATE([<level>[, <member>]])
  //                         | QTD([<member>])
  //                         | TOGGLEDRILLSTATE(<set1>, <set2>[, RECURSIVE])
                                          PERIODSTODATE~"("~opt(level~opt(","~member))~")" |
                                          QTD~"("~opt(member)~")" |
                                          TOGGLEDRILLSTATE~"("~set~","~set~opt(","~RECURSIVE)~")" |
  //                         | TOPCOUNT(<set>, <index>
  //                           [, <numeric_value_expression>])
  //                         | TOPPERCENT(<set>, <percentage>,
  //                           <numeric_value_expression>)
  //                         | TOPSUM(<set>, <numeric_value_expression>,
  //                           <numeric_value_expression>)
  //                         | UNION(<set>, <set> [, [ALL]])
  //                         | WTD([<member>])
  //                         | YTD(<member>)
                                          TOPCOUNT~"("~set~","~index~opt(","~numeric_value_expression)~")" |
                                          TOPPERCENT~"("~set~","~percentage~","~numeric_value_expression~")" |
                                          TOPSUM~"("~set~","~numeric_value_expression~","~numeric_value_expression~")" |
                                          UNION~"("~set~","~set~opt(","~ALL)~")" |
                                          WTD~"("~opt(member)~")" |
                                          YTD~"("~member~")"  
                                          
  //<desc_flags> ::= SELF
  //               | AFTER
  //               | BEFORE
  //               | BEFORE_AND_AFTER
  //               | SELF_AND_AFTER
  //               | SELF_AND_BEFORE
  //               | SELF_BEFORE_AFTER
  def desc_flags: Parser[Any] = SELF | AFTER | BEFORE | BEFORE_AND_AFTER |
  								SELF_AND_AFTER | SELF_AND_BEFORE | SELF_BEFORE_AFTER
  								
  								
  /** MDX Member Value Expressions */

  //<member_value_expression> ::= <member>.{PARENT | FIRSTCHILD | LASTCHILD
  //                            | PREVMEMBER | NEXTMEMBER}
  //                            | <member>.LEAD(<index>)
  //                            | <member>.LAG(<index>)  	
  def member_value_expression: Parser[Any] = member~"."~(PARENT | FIRSTCHILD | LASTCHILD | PREVMEMBER | NEXTMEMBER) | 
                                             member~"."~LEAD~"("~index~")" |
                                             member~"."~LAG~"("~index~")" |
                                             // NOTE:  LAG(<index>) is the same as LEAD(–<index>)	
  //                            | <member>.{FIRSTSIBLING | LASTSIBLING}
  //                            | <dimension>[.CURRENTMEMBER]
  //                            | <dimension>.DEFAULTMEMBER
  //                            | <hierarchy>.DEFAULTMEMBER
  //                            | ANCESTOR(<member>, <level>)
  //                            | CLOSINGPERIOD(<level>[, <member>])
  //                            | COUSIN(<member>, <member>)
  //                            | OPENINGPERIOD(<level>[, <member>])
  //                            | PARALLELPERIOD([<level>[, <index>
  //                                             [, <member>]]])						
                                             member~"."~(FIRSTSIBLING | LASTSIBLING) |
                                             dimension~opt("."~CURRENTMEMBER) |
                                             dimension~"."~DEFAULTMEMBER |
                                             hierarchy~"."~DEFAULTMEMBER |
                                             ANCESTOR~"("~member~","~level~")" |
                                             CLOSINGPERIOD~"("~level~opt(","~member)~")" |
                                             COUSIN~"("~member~","~member~")" |
                                             OPENINGPERIOD~"("~level~opt(","~member)~")" |
                                             PARALLELPERIOD~"("~level~opt(","~index~opt(","~member))~")"						
 
  /** MDX Tuple Value Expressions */
                                             
  // <tuple_value_expression> ::= <set>.CURRENTMEMBER
  //                           | <set>[.ITEM]({<string_value_expression>
  //                                       [, <string_value_expression>...]}
  //                                       | <index>)
  def tuple_value_expression: Parser[Any] = set~"."~CURRENTMEMBER |
                                            set~opt("."~ITEM)~"("~(repsep(string_value_expression, ",") | index)~")"
                                            
  /** MDX Conditional Expressions */
                                            
    /** IF Clause */
                                            
  //<if_expression> ::= IIF(<search_condition>, <true_part>, <false_part>)
  def if_expression: Parser[Any] = IIF~"("~search_condition~","~true_part~","~false_part~")"
  
  //<true_part> ::= <value_expression>
  def true_part: Parser[Any] = value_expression
  
  //<false_part> ::= <value_expression>                                            
  def false_part: Parser[Any] = value_expression                                            
                                 
  
    /** Simple Case Expression */
  //<simple_case> ::= CASE <case_operand>
  //                       <simple_when_clause>...
  //                       [ <else_clause> ]
  //                  END
  def simple_case: Parser[Any] = CASE~case_operand~
                                   repsep(simple_when_clause, whiteSpace)~
                                   opt(else_clause)~
                                 END

  //<simple_when_clause> ::= WHEN <when_operand> THEN <result>
  def simple_when_clause: Parser[Any] = WHEN~when_operand~THEN~result

  //<else_clause> ::= ELSE <value_expression>
  def else_clause: Parser[Any] = ELSE~value_expression

  //<case_operand> ::= <value_expression>
  def case_operand: Parser[Any] = value_expression

  //<when_operand> ::= <value_expression>
  def when_operand: Parser[Any] = value_expression

  //<result> ::= <value_expression>
  def result: Parser[Any] = value_expression
                                            
    /** Searched Case Expression */
  //<searched_case> ::= CASE
  //                    <searched_when_clause>...
  //                    [ <else_clause> ]
  //                    END
  def searched_case: Parser[Any] = CASE~
                                     repsep(searched_when_clause, whiteSpace)~
                                     opt(else_clause)~
                                   END

  //<searched_when_clause> ::= WHEN <search_condition> THEN <result>
  def searched_when_clause: Parser[Any] = WHEN~search_condition~THEN~result
  
  /** MDX Numeric Value Functions */
  //<numeric_value_function> ::=
  //AGGREGATE(<set> [, <numeric_value_expression>])
  //AVG(<set>[, <numeric_value_expression>])
  //CORRELATION(<set> , <numeric_value_expression>
  //      [, <numeric_value_expression>])
  //COVARIANCE(<set>, <numeric_value_expression>
  //      [, <numeric_value_expression>])
  //COUNT(<set>[, INCLUDEEMPTY])
  //LINREGINTERCEPT(<set>, <numeric_value_expression>
  //      [, <numeric_value_expression>])
  //LINREGPOINT(<numeric_value_expression>, <set>,
  //      <numeric_value_expression> [, <numeric_value_expression>])
  //LINREGR2(<set>, <numeric_value_expression>
  //      [, <numeric_value_expression>])
  //LINREGSLOPE(<set>, <numeric_value_expression>
  //      [, <numeric_value_expression>])
  //LINREGVARIANCE(<set>, <numeric_value_expression>
  //      [, <numeric_value_expression>])
  //MAX(<set>[, <numeric_value_expression>])
  //MEDIAN(<set>[, <numeric_value_expression>])
  //MIN(<set>[, <numeric_value_expression>])
  //RANK(<tuple>, <set>)
  //STDEV(<set>[, <numeric_value_expression>])
  //SUM(<set>[, <numeric_value_expression>])
  //VAR(<set>[, <numeric_value_expression>])
  def numeric_value_function: Parser[Any] = AGGREGATE~"("~set~opt(","~numeric_value_expression)~")" |
                                            AVG~"("~set~opt(","~numeric_value_expression)~")" |
                                            CORRELATION~"("~set~","~numeric_value_expression~
                                                  opt(","~numeric_value_expression)~")" |
                                            COVARIANCE~"("~set~","~numeric_value_expression~
                                                  opt(","~numeric_value_expression)~")" |
                                            COUNT~"("~set~opt(","~INCLUDEEMPTY) |
                                            LINREGINTERCEPT~"("~set~","~numeric_value_expression~
                                                  opt(","~numeric_value_expression)~")" |
                                            LINREGPOINT~"("~numeric_value_expression~","~set~","~
                                                  numeric_value_expression~opt(","~numeric_value_expression)~")" |
                                            LINREGR2~"("~set~","~numeric_value_expression~
                                                  opt(","~numeric_value_expression)~")" |
                                            LINREGSLOPE~"("~set~","~numeric_value_expression~
                                                  opt(","~numeric_value_expression)~")" |
                                            LINREGVARIANCE~"("~set~","~numeric_value_expression~
                                                  opt(","~numeric_value_expression)~")" |
                                            MAX~"("~set~opt(","~numeric_value_expression)~")" |
                                            MEDIAN~"("~set~opt(","~numeric_value_expression)~")" |
                                            MIN~"("~set~opt(","~numeric_value_expression)~")" |
                                            RANK~"("~tuple~","~set~")" |
                                            STDEV~"("~set~opt(","~numeric_value_expression)~")" |
                                            SUM~"("~set~opt(","~numeric_value_expression)~")" |
                                            VAR~"("~set~opt(","~numeric_value_expression)~")"
                                            
  /** MDX Search Condition */ 
                                      
  //<search_condition> ::= <boolean_term>
  //                     | <search_condition> {OR | XOR} <boolean_term>
  def search_condition: Parser[Any] = boolean_term |
                                      search_condition~(OR | XOR)~boolean_term

  //<boolean_term> ::= <boolean_factor> | <boolean_term> AND <boolean_factor>
  def boolean_term: Parser[Any] = boolean_factor | boolean_term~AND~boolean_factor

  //<boolean_factor> ::= [NOT] <boolean_primary>
  def boolean_factor: Parser[Any] = opt(NOT)~boolean_primary

  //<boolean_primary> ::= <value_expression> <comp_op> <value_expression>
  //                    | ISEMPTY(<value_expression>)
  //                    | (<search_condition>)
  def boolean_primary: Parser[Any] = value_expression~comp_op~value_expression |
                                     ISEMPTY~"("~value_expression~")" |
                                     "("~search_condition~")"

  //<comp_op> ::= <equals_operator>
  //            | <not_equals_operator>
  //            | <less_than_operator>
  //            | <greater_than_operator>
  //            | <less_than_or_equals_operator>
  //            | <greater_than_or_equals_operator>
  def comp_op: Parser[Any] = equals_operator |
                             not_equals_operator |
                             less_than_operator |
                             greater_than_operator |
                             less_than_or_equals_operator |
                             greater_than_or_equals_operator

  //<equals_operator> ::= =
  def equals_operator: Parser[String] = "="

  //<not_equals_operator> ::= <>
  def not_equals_operator: Parser[String] = "<>"

  //<greater_than_operator> ::= >
  def greater_than_operator: Parser[String] = ">"

  //<less_than_operator> ::= <
  def less_than_operator: Parser[String] = "<"

  //<greater_than_or_equals_operator> ::= >=
  def greater_than_or_equals_operator: Parser[String] = ">="

  //<less_than_or_equals_operator> ::= <=
  def less_than_or_equals_operator: Parser[String] = "<="
    
  /** MDX Statement */
  //<MDX_statement> ::= <select_statement>
  //                  | <create_formula_statement>
  //                  | <drop_formula_statement>
  def mdx_statement: Parser[Any] = select_statement |
                                   create_formula_statement |
                                   drop_formula_statement

  //<select_statement> ::= [WITH <formula_specification>]
  //                        SELECT [<axis_specification>
  //                               [, <axis_specification>...]]
  //                        FROM [<cube_specification>]
  //                        [WHERE [<slicer_specification>]]
  //                        [<cell_props>]
  def select_statement: Parser[Any] = opt(WITH~formula_specification)
                                      SELECT~repsep(axis_specification, ",")~
                                      FROM~opt(cube_specification)~
                                      opt(WHERE~opt(slicer_specification))
                                      opt(cell_props)
  

  //<formula_specification> ::= <single_formula_specification>
  //                           [<single_formula_specification>...]
  def formula_specification: Parser[Any] = repsep(single_formula_specification, whiteSpace)

  //<single_formula_specification> ::= <member_specification>
  //                                 | <set_specification>
  def single_formula_specification: Parser[Any] = member_specification | set_specification

  //<member_specification> ::= MEMBER <member_name> AS <value_expression>
  //                           [, <solve_order_specification>]
  //                           [, <member_property_definition>...]
  def member_specification: Parser[Any] = MEMBER~member_name~AS~value_expression~
                                          opt(", "~solve_order_specification)~
                                          opt(","~repsep(member_property_definition, ","))

  //<member_name> ::= <member>.<identifier>
  //                | <cube_name>.<member>.<identifier>
  def member_name: Parser[Any] = member~"."~identifier |
                                 cube_name~"."~member~"."~identifier

  //<solve_order_specification> ::= SOLVE_ORDER = <unsigned_integer>
  def solve_order_specification: Parser[Any] = SOLVE_ORDER~"="~unsigned_integer

  //<member_property_definition> ::= <identifier> = <value_expression>
  def member_property_definition: Parser[Any] = identifier~"="~value_expression
  
  //<set_specification> ::= SET <set_name> AS <set>
  def set_specification: Parser[Any] = SET~set_name~AS~set
  
  //<set_name> ::= <identifier> | <cube_name>.<identifier>
  def set_name: Parser[Any] = identifier | cube_name~"."~identifier
  
  //<axis_specification> ::= [NON EMPTY] <set> [<dim_props>] ON <axis_name>
  def axis_specification: Parser[Any] = opt(NON_EMPTY)~set~opt(dim_props)~ON~axis_name

  //<axis_name> ::= COLUMNS
  //              | ROWS
  //              | PAGES
  //              | CHAPTERS
  //              | SECTIONS
  //              | AXIS(<index>)
  def axis_name: Parser[Any] = COLUMNS | ROWS | PAGES | CHAPTERS | SECTIONS | AXIS~"("~index~")"

  //<dim_props> ::= [DIMENSION] PROPERTIES <property> [, <property>...]
  def dim_props: Parser[Any] = opt(DIMENSION)~PROPERTIES~repsep(property, ",")

  //<cube_specification> ::= [<cube_name> [,<cube_name>...]]
  def cube_specification: Parser[Any] = repsep(cube_name, ",")

  //<slicer_specification> ::= {<set> | <tuple>}
  def slicer_specification: Parser[Any] = set | tuple

  //<cell_props> ::= [CELL] PROPERTIES <cell_property> [, <cell_property>...]
  def cell_props: Parser[Any] = opt(CELL)~PROPERTIES~repsep(cell_property, ",")

  //<cell_property> ::= <mandatory_cell_property>
  //                  | <optional_cell_property>
  //                  | <provider_specific_cell_property>
  def cell_property: Parser[Any] = mandatory_cell_property |
                                   optional_cell_property |
                                   provider_specific_cell_property

  //<mandatory_cell_property> ::= CELL_ORDINAL | VALUE | FORMATTED_VALUE
  def mandatory_cell_property: Parser[Any] = CELL_ORDINAL | VALUE | FORMATTED_VALUE

  //<optional_cell_property> ::= FORMAT_STRING
  //                           | FORE_COLOR
  //                           | BACK_COLOR
  //                           | FONT_NAME
  //                           | FONT_SIZE
  //                           | FONT_FLAGS
  def optional_cell_property: Parser[Any] = FORMAT_STRING | FORE_COLOR | BACK_COLOR |
                                            FONT_NAME | FONT_SIZE | FONT_FLAGS

  //<provider_specific_cell_property> ::= <identifier>
  def provider_specific_cell_property: Parser[Any] = identifier

  //<create_formula_statement> ::= CREATE [<scope>]<formula_specification>
  def create_formula_statement: Parser[Any] = CREATE~opt(scope)~formula_specification

  //<drop_formula_statement> ::= <drop_member_statement>
  //                           | <drop_set_statement>
  def drop_formula_statement: Parser[Any] = drop_member_statement | drop_set_statement

  //<drop_member_statement> ::= DROP MEMBER <member_name>
  //                                     [, <member_name>...]
  def drop_member_statement: Parser[Any] = DROP~MEMBER~repsep(member_name, ",")

  //<drop_set_statement> ::= DROP SET <set_name> [, <set_name>...]
  def drop_set_statement: Parser[Any] = DROP~SET~repsep(set_name, ",")

  //<scope> := GLOBAL | SESSION
  def scope: Parser[Any] = GLOBAL | SESSION
                                             
}

object MDX {
  
  val DIMENSION = "DIMENSION"
  val HIERARCHY = "HIERARCHY"
  val LEVELS = "LEVELS"
  val MEMBERS = "MEMBERS"
  val CHILDREN = "CHILDREN"
  val BOTTOMCOUNT = "BOTTOMCOUNT"
  val BOTTOMSUM = "BOTTOMSUM"
  val CROSSJOIN = "CROSSJOIN"
  val DESCENDANTS = "DESCENDANTS"
    
  val CATALOG_NAME = "CATALOG_NAME"
  val SCHEMA_NAME = "SCHEMA_NAME" 
  val CUBE_NAME = "CUBE_NAME"
  val DIMENSION_UNIQUE_NAME = "DIMENSION_UNIQUE_NAME"
  val HIERARCHY_UNIQUE_NAME = "HIERARCHY_UNIQUE_NAME"
  val LEVEL_UNIQUE_NAME = "LEVEL_UNIQUE_NAME"
  val LEVEL_NUMBER = "LEVEL_NUMBER"
  val MEMBER_UNIQUE_NAME = "MEMBER_UNIQUE_NAME"
  val MEMBER_NAME = "MEMBER_NAME"
  val MEMBER_TYPE = "MEMBER_TYPE"
  val MEMBER_GUID = "MEMBER_GUID"
  val MEMBER_CAPTION = "MEMBER_CAPTION"
  val MEMBER_ORDINAL = "MEMBER_ORDINAL"
  val CHILDREN_CARDINALITY = "CHILDREN_CARDINALITY"
  val PARENT_LEVEL = "PARENT_LEVEL"
  val PARENT_UNIQUE_NAME = "PARENT_UNIQUE_NAME"
  val PARENT_COUNT = "PARENT_COUNT"
  val DESCRIPTION = "DESCRIPTION"
    
  val SELF = "SELF"
  val AFTER = "AFTER"
  val BEFORE = "BEFORE"
  val BEFORE_AND_AFTER = "BEFORE_AND_AFTER"
  val SELF_AND_AFTER = "SELF_AND_AFTER"
  val SELF_AND_BEFORE = "SELF_AND_BEFORE"
  val SELF_BEFORE_AFTER = "SELF_BEFORE_AFTER"
    
  val PARENT = "PARENT"
  val FIRSTCHILD = "FIRSTCHILD"
  val LASTCHILD = "LASTCHILD"
  val PREVMEMBER = "PREVMEMBER"
  val NEXTMEMBER = "NEXTMEMBER"
  val LEAD = "LEAD"
  val LAG = "LAG"
  val FIRSTSIBLING = "FIRSTSIBLING"
  val LASTSIBLING = "LASTSIBLING"
  val CURRENTMEMBER = "CURRENTMEMBER"
  val DEFAULTMEMBER = "DEFAULTMEMBER"
  val ANCESTOR = "ANCESTOR"
  val CLOSINGPERIOD = "CLOSINGPERIOD"
  val COUSIN = "COUSIN"
  val OPENINGPERIOD = "OPENINGPERIOD"
  val PARALLELPERIOD = "PARALLELPERIOD"
    
  val ITEM = "ITEM"
    
  val IIF = "IIF"
    
  val CASE = "CASE"
  val END = "END"
  val WHEN = "WHEN"
  val ELSE = "ELSE"
  val THEN = "THEN"
    
  val DISTINCT = "DISTINCT"
  val DRILLDOWNLEVEL = "DRILLDOWNLEVEL"
  val DRILLDOWNLEVELBOTTOM = "DRILLDOWNLEVELBOTTOM"
  val DRILLDOWNLEVELTOP = "DRILLDOWNLEVELTOP"
  val DRILLDOWNMEMBER = "DRILLDOWNMEMBER"
  val DRILLDOWNMEMBERBOTTOM = "DRILLDOWNMEMBERBOTTOM"
  val DRILLDOWNMEMBERTOP = "DRILLDOWNMEMBERTOP"
  val DRILLUPLEVEL = "DRILLUPLEVEL"
  val DRILLUPMEMBER = "DRILLUPMEMBER"
  val EXCEPT = "EXCEPT"
  val EXTRACT = "EXTRACT"
  val FILTER = "FILTER"
  val GENERATE = "GENERATE"
  val HIERARCHIZE = "HIERARCHIZE"
  val INTERSECT = "INTERSECT"
  val LASTPERIODS = "LASTPERIODS"
  val MTD = "MTD"
  val ORDER = "ORDER"
  val RECURSIVE = "RECURSIVE"
  val ALL = "ALL"
  val ASC = "ASC"
  val DESC = "DESC"
  val BASC = "BASC"
  val BDESC = "BDESC"
  val PERIODSTODATE = "PERIODSTODATE"
  val QTD = "QTD"
  val TOGGLEDRILLSTATE = "TOGGLEDRILLSTATE"
  val TOPCOUNT = "TOPCOUNT"
  val TOPPERCENT = "TOPPERCENT"
  val TOPSUM = "TOPSUM"
  val UNION = "UNION"
  val WTD = "WTD"
  val YTD = "YTD"    
    
  val COALESCEEMPTY = "COALESCEEMPTY"
  val E = "E"
  val VALUE = "VALUE"
    
  val AGGREGATE = "AGGREGATE"
  val AVG = "AVG"
  val CORRELATION = "CORRELATION"
  val COVARIANCE = "COVARIANCE"
  val COUNT = "COUNT"
  val LINREGINTERCEPT = "LINREGINTERCEPT"
  val LINREGPOINT = "LINREGPOINT"
  val LINREGR2 = "LINREGR2"
  val LINREGSLOPE = "LINREGSLOPE"
  val LINREGVARIANCE = "LINREGVARIANCE"
  val MAX = "MAX"
  val MEDIAN = "MEDIAN"
  val MIN = "MIN"
  val RANK = "RANK"
  val STDEV = "STDEV"
  val SUM = "SUM"
  val VAR = "VAR"
  val INCLUDEEMPTY = "INCLUDEEMPTY"
    
   
  val OR = "OR"
  val XOR = "XOR"
  val AND = "AND"
  val ISEMPTY = "ISEMPTY"
  val NOT = "NOT"
    
  val WITH = "WITH"
  val SELECT = "SELECT"
  val FROM = "FROM"
  val WHERE = "WHERE"
  val MEMBER = "MEMBER"
  
  val AS = "AS"
  val SOLVE_ORDER = "SOLVE_ORDER"
  val SET = "SET"
  val NON_EMPTY = "NON EMPTY"
  val ON = "ON"
  val COLUMNS = "COLUMNS"
  val ROWS = "ROWS"
  val PAGES = "PAGES"
  val CHAPTERS = "CHAPTERS"
  val SECTIONS = "SECTIONS"
  val AXIS = "AXIS"
  val PROPERTIES = "PROPERTIES"
  val CELL = "CELL"
  val CELL_ORDINAL = "CELL_ORDINAL"
  val FORMATTED_VALUE = "FORMATTED_VALUE"
  val FORMAT_STRING = "FORMAT_STRING"
  val FORE_COLOR = "FORE_COLOR"
  val BACK_COLOR = "BACK_COLOR"
  val FONT_NAME = "FONT_NAME"
  val FONT_SIZE = "FONT_SIZE"
  val FONT_FLAGS = "FONT_FLAGS"
  val CREATE = "CREATE"
  val DROP = "DROP"
  val GLOBAL = "GLOBAL"
  val SESSION = "SESSION"
    
}