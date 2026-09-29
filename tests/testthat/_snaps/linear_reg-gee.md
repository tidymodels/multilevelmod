# gee_formula() errors informatively on wrong number of id vars

    Code
      multilevelmod:::gee_formula(depr_score ~ week)
    Condition
      Error in `multilevelmod:::gee_formula()`:
      ! There should be a single id column specified using the `id_var()` function.
      i For example: `y ~ x + id_var(id_col)`

---

    Code
      multilevelmod:::gee_formula(depr_score ~ week + id_var(subject) + id_var(week))
    Condition
      Error in `multilevelmod:::gee_formula()`:
      ! There should be a single id column specified using the `id_var()` function.
      i For example: `y ~ x + id_var(id_col)`

