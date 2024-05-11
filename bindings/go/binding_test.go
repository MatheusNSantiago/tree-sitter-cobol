package tree_sitter_cobol_test

import (
	"testing"

	tree_sitter "github.com/smacker/go-tree-sitter"
	"github.com/tree-sitter/tree-sitter-cobol"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_cobol.Language())
	if language == nil {
		t.Errorf("Error loading Cobol grammar")
	}
}
