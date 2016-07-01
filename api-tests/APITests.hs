{-# OPTIONS_GHC -F -pgmF htfpp #-}

import qualified Cases
import           Prelude
import           Test.Framework

main = htfMain $ htf_thisModulesTests


test_spinalizeCamelCase = do
  assertEqual "abc-def" $ Cases.spinalize "abcDef"
  assertEqual "abc-def" $ Cases.spinalize "abcDEF"
  assertEqual "my-html-processor" $ Cases.spinalize "myHTMLProcessor"

test_spinalizeSymbols = do
  assertEqual "abc-def" $ Cases.spinalize "abc_def"
  assertEqual "abc-def" $ Cases.spinalize "abc-def"
  assertEqual "abc-def" $ Cases.spinalize "abc/def"
  assertEqual "abc-def" $ Cases.spinalize "abc,def"
  assertEqual "abc-def" $ Cases.spinalize "abc.def"
  assertEqual "abc-def" $ Cases.spinalize "abc:def"

test_spinalizeMultipleDelimiters = do
  assertEqual "abc-def" $ Cases.spinalize "abc_-:,/def"
  assertEqual "abc-def" $ Cases.spinalize "abc /-def"

test_spinalizeSpaces = do
  assertEqual "abc-def" $ Cases.spinalize "abc def"
  assertEqual "abc-def" $ Cases.spinalize "abc\ndef"
  assertEqual "abc-def" $ Cases.spinalize "abc\rdef"
  assertEqual "abc-def" $ Cases.spinalize "abc\r\n\rdef"
  assertEqual "abc-def" $ Cases.spinalize "abc\tdef"
  assertEqual "abc-def" $ Cases.spinalize "abc  def"

test_spinalizeTrimming = do
  assertEqual "abc-def" $ Cases.spinalize " abc def "
  assertEqual "abc-def" $ Cases.spinalize "/abc/def/"

test_spinalizeNumbers = do
  assertEqual "abc-12" $ Cases.spinalize "abc12"

test_spinalizeUnicode = do
  assertEqual "ёжик-лижет-мёд" $ Cases.spinalize "Ёжик лижет мёд."

test_snakify = do
  assertEqual "ёжик_лижет_мёд" $ Cases.snakify "Ёжик лижет мёд."

test_camelize = do
  assertEqual "abcDef" $ Cases.camelize "abc-def"
  assertEqual "AbcDef" $ Cases.camelize "Abc-def"
  assertEqual "parseDbmXml" $ Cases.camelize "parse DBM XML"

  assertEqual "AbcDef" $ Cases.camelize "Abc_Def"
  assertEqual "AbcDef" $ Cases.camelize "Abc_DEF" -- change this behaviour
  assertEqual "AbcDeF" $ Cases.camelize "Abc_DeF"
  assertEqual "abcDef" $ Cases.camelize "abc_-:,/def"
  assertEqual "abcDef" $ Cases.camelize "abc /-def"
  assertEqual "InternalSpecialCharactersAreRemoved" $ Cases.camelize "Internal special characters ?)@ are removed"

test_humanize = do
  assertEqual "Abc Def" $ Cases.humanize "abc-def"
  assertEqual "Abc Def" $ Cases.humanize "Abc-def"
  assertEqual "Parse Dbm Xml" $ Cases.humanize "parse DBM XML"
