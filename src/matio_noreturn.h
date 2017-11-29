#ifndef MATIO_NORETURN_H
#define MATIO_NORETURN_H

/* 
  The following macros handle noreturn attributes according to the latest
  C11/C++11 standard with fallback to GNU, Clang or MSVC extensions if using
  an older compiler.
*/

#if __STDC_VERSION__ >= 201112L
#define MATIO_NORETURN _Noreturn
#define MATIO_NORETURNATTR
#elif __cplusplus >= 201103L
#if (defined(__GNUC__) && __GNUC__ >= 5) || \
    (defined(__GNUC__) && defined(__GNUC_MINOR__) && __GNUC__ == 4 && __GNUC_MINOR__ >= 8)
#define MATIO_NORETURN [[noreturn]]
#define MATIO_NORETURNATTR
#elif (defined(__GNUC__) && __GNUC__ >= 3) || \
      (defined(__GNUC__) && defined(__GNUC_MINOR__) && __GNUC__ == 2 && __GNUC_MINOR__ >= 8)
#define MATIO_NORETURN
#define MATIO_NORETURNATTR __attribute__((noreturn))
#elif defined(__GNUC__)
#define MATIO_NORETURN
#define MATIO_NORETURNATTR
#else
#define MATIO_NORETURN [[noreturn]]
#define MATIO_NORETURNATTR
#endif
#elif defined(__clang__)
#if __has_attribute(noreturn)
#define MATIO_NORETURN
#define MATIO_NORETURNATTR __attribute__((noreturn))
#else
#define MATIO_NORETURN
#define MATIO_NORETURNATTR
#endif
#elif (defined(__GNUC__) && __GNUC__ >= 3) || \
      (defined(__GNUC__) && defined(__GNUC_MINOR__) && __GNUC__ == 2 && __GNUC_MINOR__ >= 8) || \
      (defined(__SUNPRO_C) && __SUNPRO_C >= 0x5110)
#define MATIO_NORETURN
#define MATIO_NORETURNATTR __attribute__((noreturn))
#elif (defined(_MSC_VER) && _MSC_VER >= 1200) || \
       defined(__BORLANDC__)
#define MATIO_NORETURN __declspec(noreturn)
#define MATIO_NORETURNATTR
#else
#define MATIO_NORETURN
#define MATIO_NORETURNATTR
#endif

#endif /* !MATIO_NORETURN_H */
