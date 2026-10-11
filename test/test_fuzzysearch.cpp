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

  assert(fuzzy_search_fold("B" "\xC3\x96" "S") == "b" "\xC3\xB6" "s"); // Ö -> ö
  assert(fuzzy_search_fold("\xC5\x81" "\xC3\xB3" "d" "\xC5\xBA") == "\xC5\x82" "\xC3\xB3" "d" "\xC5\xBA"); // Łódź -> łódź
  assert(fuzzy_search_fold("\xC5\xB8") == "\xC3\xBF"); // Ÿ -> ÿ
  assert(fuzzy_search_fold("a[1]{2}") == "a[1]{2}"); // Punctuation and digits are preserved.
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

  assert(fuzzy_search_match("OB-Xd", "obxd"));
  assert(fuzzy_search_match("OB-Xd", "OBXD"));
  assert(!fuzzy_search_match("OB-Xd", "obxdzz"));
  assert(fuzzy_search_match("Reverb (Faust)", "faust rev"));
  assert(fuzzy_search_match("Reverb (Faust)", "rev faust"));
  assert(!fuzzy_search_match("Reverb (Faust)", "faustxyz"));
  assert(fuzzy_search_match("Reverb (Faust)", "   "));
  assert(fuzzy_search_match("B" "\xC3\xB6" "sendorfer", "B" "\xC3\x96" "S"));
  assert(fuzzy_search_match("\xC5\x81" "\xC3\xB3" "d" "\xC5\xBA", "\xC5\x82" "\xC3\xB3" "d"));
  assert(fuzzy_search_match("Bass", "bs"));
  assert(!fuzzy_search_match("Bass", "sb"));
  assert(fuzzy_search_match("String Bass", "sb"));
  assert(fuzzy_search_match("FluidSynth", "fls"));
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

  // A prefix match should score higher than a scattered match.
  assert(fuzzy_search_score("Reverb", "rev") > fuzzy_search_score("Very Early Version", "rev"));

  // Non-matching query scores -1, empty query scores >= 0.
  assert(fuzzy_search_score("Bass", "sb") == -1);
  assert(fuzzy_search_score("Bass", "") >= 0);

  // Convenience wrapper.
  assert(fuzzy_search_match("Reverb", "rev"));
}

int main(void){
  test_fold();
  test_empty_query();
  test_matches();
  test_score();

  printf("Success, no errors\n");

  return 0;
}
