formr_api_access_token(
	client_id = "0c78f991159e27370bbf0e9abdc9713a",
	client_secret = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9IjBjNzhmOTkxMTU5ZTI3Mzcw",
	host = "http://api.localhost/"
)

x <- formr_get_results("test2",
											 surveys = list(platzhalter = "nav1"),
											 sessions = "HGRDJY6mDzpaCrCI7jKh5JeFGqH8RiOBxKv6HmIa7DxqlOEaNwdgwgbHyVwKHRGv")

y <- formr_get_results("test2")
