#include <stdio.h>
#include <assert.h>

#include "../common/FuzzySearch.h"

using namespace radium;


static void test_fold(void){
  assert(fuzzy_search_fold("Open File") == "open file");
  assert(fuzzy_search_fold("ABC-xyz") == "abc-xyz");
  assert(fuzzy_search_fold("") == "");

  // Latin-1 and Latin Extended-A folding.
  assert(fuzzy_search_fold(QString(QChar(0xC5))) == QString(QChar(0xE5))); // Å -> å
  assert(fuzzy_search_fold(QString(QChar(0x100))) == QString(QChar(0x101))); // Ā -> ā
  assert(fuzzy_search_fold(QString(QChar(0x130))) == "i"); // İ -> i
}

static void test_empty_query(void){
  FuzzySearchQuery empty("");
  assert(empty.empty());
  assert(empty.matches("whatever"));
  assert(empty.score("whatever") == 0);

  FuzzySearchQuery only_whitespace(" \t\n ");
  assert(only_whitespace.empty());
  assert(only_whitespace.matches("whatever"));
}

static void test_matches(void){
  assert(fuzzy_search_match("Open File...", "open"));
  assert(fuzzy_search_match("Open File...", "file"));
  assert(fuzzy_search_match("Open File...", "of")); // Subsequence across words.
  assert(fuzzy_search_match("Open File...", "OPN")); // Case insensitive.
  assert(fuzzy_search_match("Open File...", "file open")); // Tokens can be in any order.

  assert(!fuzzy_search_match("Open File...", "openx"));
  assert(!fuzzy_search_match("Open File...", "file open z"));
  assert(!fuzzy_search_match("", "x"));
  assert(fuzzy_search_match("anything", ""));
}

static void test_score(void){
  // A match at the start of the text should score higher than a match in the middle.
  assert(fuzzy_search_score("Open", "open") > fuzzy_search_score("ReOpen", "open"));

  // Matches at word boundaries should score higher than matches inside a word.
  assert(fuzzy_search_score("Open File", "of") > fuzzy_search_score("Open File", "oe"));

  // A match at the start of the text should score higher than a later match.
  assert(fuzzy_search_score("File", "file") > fuzzy_search_score("Open File", "file"));

  // Non-matches score -1
  assert(fuzzy_search_score("Open File", "xyz") == -1);
  assert(fuzzy_search_score("", "x") == -1);

  // Matches score a positive number.
  assert(fuzzy_search_score("Open File", "open") > 0);
  assert(fuzzy_search_score("Open File", "of") > 0);
}

int main(void){
  test_fold();
  test_empty_query();
  test_matches();
  test_score();

  printf("Success, no errors\n");

  return 0;
}
