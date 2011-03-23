// autologin.c
//
// Automatically login into a virtual console with a hardcoded
// username.

#include <unistd.h>

int main() {
  execlp("login", "login", "-f", "joe", 0);
}


