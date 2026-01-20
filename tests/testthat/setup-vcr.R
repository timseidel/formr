library(vcr)

# Only configure VCR if the environment variable exists
if (requireNamespace("vcr", quietly = TRUE) && Sys.getenv("FORMR_HOST") != "") {
	
	# Clean the host to get ONLY the domain
	host_domain <- gsub("^https?://", "", Sys.getenv("FORMR_HOST"))
	
	vcr::vcr_configure(
		dir = "../fixtures/vcr_cassettes",
		
		filter_sensitive_data = list(
			"formr-client-id-redacted"     = Sys.getenv("FORMR_CLIENT_ID"),
			"formr-client-secret-redacted" = Sys.getenv("FORMR_CLIENT_SECRET"),
			"formr-host-redacted"          = host_domain
		)
	)
}