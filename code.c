#include <stdio.h>
#include <stdlib.h>
#include <math.h>


#define BASE 1000000000000000000ULL;      // 1e18


struct BigIntStruct {
    short int sign;
    unsigned int len;
    unsigned long long *d;
};


typedef struct BigIntStruct* BigInt;
typedef struct BigIntStruct BigIntObj;


BigInt new_BigInt(const unsigned int length) {
    BigInt b = (BigInt) malloc(sizeof(BigIntObj));
    b->sign = 1;
    b->len = length;
    b->d = (unsigned long long*) malloc(length * sizeof(unsigned long long));
    return b;
}



int max(int x, int y) {
    return x > y ? x : y;
}



void set_zero(BigInt b) {
    for (unsigned int i = 0; i < b->len; i++)
        b->d[i] = 0;
}


void free_BigInt(BigInt b) {
    free(b->d);
    free(b);
}


void print_BigInt(BigInt b) {
    printf("%c%0llu", b->sign == 1 ? '+' : '-', b->d[b->len - 1]);
    for (int i = b->len - 2; i >= 0; i--)
        printf("%018llu", b->d[i]);
    printf("\n");
}


void increase_size(BigInt b, const unsigned int delta_len) {
    b->d = (unsigned long long*) realloc(b->d, sizeof(unsigned long long) * (b->len + delta_len));
    b->len += delta_len;
    for (int i = b->len - delta_len; i < b->len; i++)
        b->d[i] = 0;
}


void increase_size(BigInt b) {
    increase_size(b, b->len);
}


BigInt Add(const BigInt a, const BigInt b) {
    BigInt c = new_BigInt(1 + max(a->len, b->len));
    set_zero(c);
    unsigned long long carry = 0;
    for (unsigned int i = 0; i < c->len - 1; i++) {
        c->d[i] = a->d[i] + b->d[i] + carry;
        carry = c->d[i] / BASE;
        c->d[i] %= BASE;
    }
    if (carry > 0) {
        c->d[c->len - 1] = carry;
    }
    return c;
}


BigInt Subtract(const BigInt a, const BigInt b) {
    BigInt c = new_BigInt(max(a->len, b->len));
    set_zero(c);
    unsigned long long carry = 0;
    for (unsigned int i = 0; i < c->len; i++) {
        c->d[i] = a->d[i] - b->d[i] - carry;
        carry = c->d[i] / BASE;
        c->d[i] %= BASE;
    }
    return c;
}


BigInt Multiply(const BigInt a, const BigInt b) {
    BigInt c = new_BigInt(a->len + b->len);
    set_zero(c);
    for (unsigned int i = 0; i < a->len; i++) {
        unsigned long long carry = 0;
        for (unsigned int j = 0; j < b->len; j++) {
            c->d[i + j] += a->d[i] * b->d[j] + carry;
            carry = c->d[i + j] / BASE;
            c->d[i + j] %= BASE;
        }
        c->d[i + b->len] = carry;
    }
    return c;
}


void Increment(const BigInt a, const BigInt delta) {
    if (a->len <= delta->len) {
        increase_size(a, delta->len - a->len + 1);
    }
    unsigned long long carry = 0;
    for (unsigned int i = 0; i < delta->len; i++) {
        a->d[i] += delta->d[i] + carry;
        carry = a->d[i] / BASE;
        a->d[i] %= BASE;
    }
    for (unsigned int i = delta->len; i < a->len; i++) {
        a->d[i] += carry;
        carry = a->d[i] / BASE;
        a->d[i] %= BASE;
    }
}



int main() {
    BigInt x = new_BigInt(1);
    set_zero(x);

    BigInt val = new_BigInt(1);
    set_zero(val);
    val->d[0] = -1 + BASE;

    for (int i = 0; i < 100000000; i++) {
        // print_BigInt(x);
        Increment(x, val);
    }

    print_BigInt(x);

    print_BigInt(x);

    printf("%d\n", x->len);

    return 0;
}

