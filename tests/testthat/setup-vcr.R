library(vcr)

if (requireNamespace("vcr", quietly = TRUE)) {
	
	# Clean the host to get ONLY the domain
	host_domain <- gsub("^https?://", "", Sys.getenv("FORMR_HOST"))
	
	vcr::vcr_configure(
		dir = "../fixtures/vcr_cassettes",
		
		filter_sensitive_data = list(
			"formr-client-id-redacted"     = Sys.getenv("FORMR_CLIENT_ID"),
			"formr-client-secret-redacted" = Sys.getenv("FORMR_CLIENT_SECRET"),
			
			# CHANGE THIS: Use a URL-safe placeholder (no < or >)
			"formr-host-redacted"       = host_domain
		)
	)
}