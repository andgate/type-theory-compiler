#include <string.h>
#include <stdint.h>

#include <stdio.h>

int printstr(uint8_t* str)
{
    return printf("%s", str);
}

int printint(int32_t x)
{
    return printf("%d", x);
}

//  /* reverse:  reverse string s in place */
//  void reverse(uint8_t s[])
//  {
//      int i, j;
//      uint8_t c;
 
//      for (i = 0, j = strlen((char*)s)-1; i<j; i++, j--) {
//          c = s[i];
//          s[i] = s[j];
//          s[j] = c;
//      }
//  }

// uint8_t* int2str(int32_t n, uint8_t s[])
//  {
//     int32_t i, sign;

//     if ((sign = n) < 0)  /* record sign */
//         n = -n;          /* make n positive */
//     i = 0;
//     do {       /* generate digits in reverse order */
//         s[i++] = (n % 10) + '0';   /* get next digit */
//     } while ((n /= 10) > 0);     /* delete it */
//     if (sign < 0)
//         s[i++] = '-';
//     s[i] = '\0';

//     puts((char*)s);
//     reverse(s);
//     puts((char*)s);

//     return s;
//  }

char* int2str(int32_t n, char* buff) {
    sprintf(buff, "%d", n);
    return buff;
}

