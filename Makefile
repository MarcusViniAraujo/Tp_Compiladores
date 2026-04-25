# Nome do arquivo principal (sem a extensão .tex)
DOCNAME = relatorio

# Pasta onde está o arquivo .tex
DIR = relatorio

# Compilador
LATEX = pdflatex

# Regra padrão (o que acontece se você digitar apenas 'make')
all: pdf

# Regra para gerar o PDF
# Entramos na pasta (cd) e rodamos o compilador lá dentro para evitar problemas de caminho
pdf:
	cd $(DIR) && $(LATEX) -interaction=nonstopmode $(DOCNAME).tex
	cd $(DIR) && $(LATEX) -interaction=nonstopmode $(DOCNAME).tex
	@echo "Build finalizada com sucesso! O PDF esta em $(DIR)/$(DOCNAME).pdf"

# Regra para limpar arquivos temporários gerados pelo LaTeX (.aux, .log, .toc, etc)
clean:
	rm -f $(DIR)/*.aux $(DIR)/*.log $(DIR)/*.out $(DIR)/*.toc $(DIR)/*.fls $(DIR)/*.fdb_latexmk
	@echo "Arquivos temporarios removidos."

# Regra para limpar tudo, inclusive o PDF final
clean-all: clean
	rm -f $(DIR)/$(DOCNAME).pdf
	@echo "PDF removido."

.PHONY: all pdf clean clean-all