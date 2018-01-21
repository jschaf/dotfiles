package main

import (
	"fmt"
	"github.com/dustin/go-humanize"
	"os"
	"strconv"
)

func main() {
	if len(os.Args) != 2 {
		os.Stderr.WriteString("human-size takes at least 1 arg.\n")
		os.Exit(1)
	}

	n, err := strconv.ParseUint(os.Args[1], 10, 64)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Unable to parse uint64 '%s'.", os.Args[1])
		os.Exit(1)
	}

	fmt.Println(humanize.Bytes(n))
}
