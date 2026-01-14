.PHONY: help dev build clean deploy serve stop

help:
	@echo "Superwell Docs - Quick Commands:"
	@echo "  dev     - Clean + start dev server (no cache)"
	@echo "  build   - Production build with minification"
	@echo "  clean   - Remove all generated files"
	@echo "  serve   - Start server without rebuilding"
	@echo "  stop    - Kill running Hugo server"

dev: stop clean
	@hugo server --disableFastRender --noHTTPCache --bind 0.0.0.0 --port 1313 --navigateToChanged

build: clean
	@hugo --gc --minify --cleanDestinationDir --ignoreCache

clean:
	@rm -rf public resources .hugo_build.lock

serve:
	@hugo server --disableFastRender --noHTTPCache --bind 0.0.0.0 --port 1313

stop:
	@-pkill -9 -f "hugo server" 2>/dev/null || true

deploy: build
	@echo "Ready at public/"

rebuild: stop clean serve
