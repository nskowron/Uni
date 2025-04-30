package main

import (
	"fmt"
	"math/rand"
	"time"
	"unicode"
)

const (
	NrOfTravelers = 15
	MinSteps      = 10
	MaxSteps      = 100
	MinDelay      = 10 * time.Millisecond
	MaxDelay      = 50 * time.Millisecond
	BoardWidth    = 15
	BoardHeight   = 15
)

var (
	StartTime = time.Now()
)

type Position struct {
	X, Y int
}

func Move_Down(pos *Position) {
	pos.Y = (pos.Y + 1) % BoardHeight
}

func Move_Up(pos *Position) {
	pos.Y = (pos.Y + BoardHeight - 1) % BoardHeight
}

func Move_Right(pos *Position) {
	pos.X = (pos.X + 1) % BoardWidth
}

func Move_Left(pos *Position) {
	pos.X = (pos.X + BoardWidth - 1) % BoardWidth
}

type Trace struct {
	TimeStamp time.Duration
	Id        int
	Position  Position
	Symbol    rune
}

func Print_Traces(traces []Trace) {
	for _, trace := range traces {
		fmt.Printf("%f %d %d %d %c\n",
			float64(trace.TimeStamp)/float64(time.Second),
			trace.Id,
			trace.Position.X,
			trace.Position.Y,
			trace.Symbol,
		)
	}
}

type Printer struct {
	traceChannel chan []Trace
	done         chan bool
}

func (p *Printer) Start() {
	p.traceChannel = make(chan []Trace, NrOfTravelers)
	p.done = make(chan bool)

	go func() {
		for i := 0; i < NrOfTravelers; i++ {
			traces := <-p.traceChannel
			Print_Traces(traces)
		}
		p.done <- true
	}()
}

type Node struct {
	enter chan bool
	leave chan bool
}

func (n *Node) Start() {
	go func() {
		for true {
			n.enter <- true
			n.leave <- true
		}
	}()
}

type Board [BoardWidth][BoardHeight]Node

func (b *Board) Start() {
	for x := 0; x < BoardWidth; x++ {
		for y := 0; y < BoardHeight; y++ {
			b[x][y] = Node{
				enter: make(chan bool),
				leave: make(chan bool),
			}
			b[x][y].Start()
		}
	}
}

type Traveler struct {
	Id        int
	Symbol    rune
	Position  Position
	Steps     int
	Traces    []Trace
	TimeStamp time.Duration
}

func (t *Traveler) Store_Trace() {
	t.Traces = append(t.Traces, Trace{
		TimeStamp: t.TimeStamp,
		Id:        t.Id,
		Position:  t.Position,
		Symbol:    t.Symbol,
	})
}

func Make_Step(position *Position) {
	direction := rand.Intn(4)
	switch direction {
	case 0:
		Move_Up(position)
	case 1:
		Move_Down(position)
	case 2:
		Move_Left(position)
	case 3:
		Move_Right(position)
	}
}

func (t *Traveler) Init(id int, symbol rune, board *Board) {
	t.Id = id
	t.Symbol = symbol

	success := false
	for !success {
		t.Position = Position{
			X: rand.Intn(BoardWidth),
			Y: rand.Intn(BoardHeight),
		}

		select {
		case <-board[t.Position.X][t.Position.Y].enter:
			success = true
		default:

		}
	}

	t.Store_Trace()
	t.Steps = MinSteps + rand.Intn(MaxSteps-MinSteps+1)
	t.TimeStamp = time.Since(StartTime)
}

func (t *Traveler) Start(printer *Printer, board *Board) {
	go func() {
		newPosition := t.Position
		success := true
		for i := 0; i < t.Steps && success; i++ {
			time.Sleep(MinDelay + time.Duration(rand.Int63n(int64(MaxDelay-MinDelay))))
			Make_Step(&newPosition)

			select {
			case <-board[newPosition.X][newPosition.Y].enter:
				<-board[t.Position.X][t.Position.Y].leave
				t.Position = newPosition
				t.Store_Trace()
				t.TimeStamp = time.Since(StartTime)
			case <-time.After(6 * MaxDelay):
				t.Symbol = unicode.ToLower(t.Symbol)
				t.Store_Trace()
				success = false
			}
		}
		printer.traceChannel <- t.Traces
	}()
}

func main() {
	var board Board
	var travelers [NrOfTravelers]Traveler
	var printer Printer

	fmt.Printf("-1 %d %d %d\n", NrOfTravelers, BoardWidth, BoardHeight)

	board.Start()
	printer.Start()

	symbol := 'A'
	for i := 0; i < NrOfTravelers; i++ {
		travelers[i].Init(i, symbol, &board)
		symbol++
	}

	for i := 0; i < NrOfTravelers; i++ {
		travelers[i].Start(&printer, &board)
	}

	<-printer.done
}
