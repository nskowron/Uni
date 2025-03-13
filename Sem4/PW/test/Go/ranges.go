package main

import "fmt"

func ranges() {
	fruits := []string{"apple", "banana", "cherry"}

	// Using both index and value
	for i, fruit := range fruits {
		fmt.Printf("Index: %d, Fruit: %s\n", i, fruit)
	}

	// Ignoring the index with _
	for _, fruit := range fruits {
		fmt.Printf("Fruit: %s\n", fruit)
	}

	// Ignoring the value with _
	for i, _ := range fruits {
		fmt.Printf("Index: %d\n", i)
	}
}
