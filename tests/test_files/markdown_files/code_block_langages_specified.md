This is a paragraph. A block of SQL follows.

``` {.sql}
-- This is a comment in the sql
select
  user_id,
  date_trunc('day', time)
from
  user_table
where
  some_stuff is not null
```

This is an intermediate paragraph. A block of R follows.

``` {.r}
# This is a comment in the R code
x <- rnorm(100)
summary(x)
hist(x)
```
