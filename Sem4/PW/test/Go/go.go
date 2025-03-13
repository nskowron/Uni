package main

import (
	"fmt"
	"time"
)

func print(done chan<- bool) { // input channel - can send
	numbers := []int{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} // another way to declare array - SLICE
	for _, i := range numbers {
		fmt.Println(i)
	}
	done <- true
	close(done)
}

func alt_print(done chan<- bool) {
	numbers := []int{2, 4, 6, 8, 10, 12, 14, 16}
	for _, i := range numbers {
		fmt.Println(i)
	}
	done <- true
	done <- true
	close(done)
}

func test_go() {
	done := make(chan bool, 0) // capacity - same as underneath
	alt_done := make(chan bool)

	go print(done)
	go alt_print(alt_done)

	select { // aka switch
	case <-done:
		time.Sleep(time.Millisecond * 100)
		fmt.Println("First function won!")
		i := 1
		for {
			_, more := <-done
			if !more {
				fmt.Println(i, " messages sent")
				break
			} else {
				i++
			}
		}
	case <-alt_done:
		time.Sleep(time.Millisecond * 100)
		fmt.Println("Secong function won!")
		i := 1
		for range alt_done { // other way of reading channel until it closes
			i++
		}
		fmt.Println(i, " messages sent")
	case <-time.After(time.Millisecond * 10):
		fmt.Println("Both lazy")
	}
}
