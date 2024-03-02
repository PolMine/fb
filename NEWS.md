## v0.1.9.9001

- Improved robustness of ct_get(): Skip posts without message, NA if
account_platorm_id is NULL
- `ct_get()` has new argument `sleep` to manually control time between API 
requests. Defaults to 10 to conform to rate limit of 6 requests per minute.

## v0.1.9

- Draft funciont `ct_list_accounts()` to retrieve accounts for a given list from
CrowTangle.

## v0.1.8

- New function `fp_read_csv()` to digest Facepager download.

