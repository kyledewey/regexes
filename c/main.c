#include "stdio.h"

// implementation from "Beautiful Code", edited by
// Oram and Wilson

int match(char* regex, char* text) {
  if (regex[0] == '^') {
    return matchhere(regex + 1, text);
  }
  do {
    if (matchhere(regex, text)) {
      return 1;
    }
  } while (*text++ != '\0');
  return 0;
}

int matchhere(char *regex, char* text) {
  if (regex[0] == '\0')
    return 1;
  if (regex[1] == '*')
    return matchstar(regex[0], regex + 2, text);
  if (regex[0] == '$' && regex[1] == '\0')
    return *text == '\0';
  if (*text != '\0' && (regex[1] == '.' || regex[0] == *text)) 
    return matchhere(regex + 1, text + 1);
  return 0;
}

int matchstar(int c, char* regex, char* text) {
  do {
    if (matchhere(regex, text))
      return 1;
  } while (*text != '\0' && (*text++ == c || c == '.'));
  return 0;
}

int main(int argc, char** argv) {
  if (argc != 3) {
    printf("Needs a regex and a string to match against\n");
  } else {
    if (match(argv[1], argv[2])) {
      printf("matches\n");
    } else {
      printf("does not match\n");
    }
  }
}
