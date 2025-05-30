#include <iostream>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <string.h>
#include <string>
#include <limits>

#include "board.hpp"

int DEPTH;

extern short board[5][5];

inline int get_valid_moves(short moves[]) {
    int c = 0;
    for(int i = 0; i < 5; ++i) {
        for(int j = 0; j < 5; ++j) {
            if(board[i][j] == 0) {
                moves[c++] = i * 10 + j;
            }
        }
    }
    return c;
}

inline int minimax(bool maximizing, int depth, int alpha, int beta) {
    if(winCheck(1)) return std::numeric_limits<int>::max() - DEPTH + depth;;
    if(winCheck(2)) return std::numeric_limits<int>::min() + DEPTH - depth;
    if(loseCheck(1)) return std::numeric_limits<int>::min() + DEPTH - depth;
    if(loseCheck(2)) return std::numeric_limits<int>::max() - DEPTH + depth;
    if(depth == 0) return evaluate_board(); // TODO: Evaluate board state

    short moves[25];
    int c = get_valid_moves(moves);
    if (c == 0) return 0; // Draw

    if(maximizing) {
        int best = std::numeric_limits<int>::min();
        while(c-- > 0) {
            int i = moves[c] / 10;
            int j = moves[c] % 10;
            board[i][j] = 1;

            int score = minimax(false, depth - 1, alpha, beta);
            board[i][j] = 0;

            best = std::max(best, score);
            alpha = std::max(alpha, best);
            if (beta <= alpha) break;
        }
        return best;
    } else {
        int best = std::numeric_limits<int>::max();
        while(c-- > 0) {
            int i = moves[c] / 10;
            int j = moves[c] % 10;
            board[i][j] = 2;

            int score = minimax(true, depth - 1, alpha, beta);
            board[i][j] = 0;

            best = std::min(best, score);
            beta = std::min(beta, best);
            if (beta <= alpha) break;
        }
        return best;
    }
}

inline short best_move(bool maximizing_player) {
    short moves[25];
    int c = get_valid_moves(moves);

    int best_score = maximizing_player ? std::numeric_limits<int>::min() : std::numeric_limits<int>::max();
    short best_move = moves[0];

    // Check for immediate win to nto waste time when win is inevitable
    // for(int d = 0; d < c; d++) {
    //     int i = moves[d] / 10;
    //     int j = moves[d] % 10;
    //     board[i][j] = maximizing_player ? 1 : 2;

    //     if(winCheck(maximizing_player ? 1 : 2)) {
    //         board[i][j] = 0;
    //         return moves[d];
    //     }

    //     board[i][j] = 0;
    // }

    // If no immediate win -> minimax
    while(c-- > 0) {
        int i = moves[c] / 10;
        int j = moves[c] % 10;
        board[i][j] = maximizing_player ? 1 : 2;

        // Check for immediate loss
        // if(loseCheck(maximizing_player ? 1 : 2)) {
        //     board[i][j] = 0;
        //     continue;
        // }

        int score = minimax(!maximizing_player, DEPTH, std::numeric_limits<int>::min(), std::numeric_limits<int>::max());

        board[i][j] = 0;

        if ((maximizing_player && score >= best_score) || (!maximizing_player && score <= best_score)) {
            best_score = score;
            best_move = moves[c];
            if( maximizing_player && score == std::numeric_limits<int>::max() || !maximizing_player && score == std::numeric_limits<int>::min()) {
                break;
            }
        }
        std::cout << "Evaluating move: " << moves[c] << " with score: " << score << std::endl;
    }
    std::cout << "=====================================\n";
    
    return best_move;
}

int main(int argc, char* argv[]) {
    if(argc != 6) {
        std::cerr << "Usage: " << argv[0] << " <IP> <port> <nr> <nick> <depth>" << std::endl;
        return -1;
    }

    DEPTH = atoi(argv[5]);

    char server_message[16] = {0};
    std::string player_message;

    // Open socket
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if(sock < 0) {
        std::cerr << "Error creating socket" << std::endl;
        return -1;
    }
    struct sockaddr_in server_addr;
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(atoi(argv[2]));
    server_addr.sin_addr.s_addr = inet_addr(argv[1]);

    // Connect to server
    if(connect(sock, (struct sockaddr*)&server_addr, sizeof(server_addr)) < 0) {
        std::cerr << "Error connecting to server" << std::endl;
        return -1;
    }
    std::cout << "Connected to server at " << argv[1] << ":" << argv[2] << std::endl;

    // Receive server message
    if(recv(sock, server_message, sizeof(server_message), 0) < 0) {
        std::cerr << "Error receiving server message" << std::endl;
        return -1;
    }

    // Send player number and nickname
    player_message = std::string(argv[3]) + " " + argv[4] + "\n";
    if(send(sock, player_message.c_str(), player_message.size(), 0) < 0) {
        std::cerr << "Error sending player info" << std::endl;
        return -1;
    }
    std::cout << "Sent player number and nickname: " << player_message;

    // Play
    short player_number = atoi(argv[3]);
    bool maximizing_player = (player_number == 1);
    bool game_over = false;
    short msg, move;
    while(!game_over) {
        // Receive move from server
        memset(server_message, 0, sizeof(server_message));
        if(recv(sock, server_message, sizeof(server_message), 0) < 0) {
            std::cerr << "Error receiving move message" << std::endl;
            return -1;
        }
        msg = atoi(server_message);
        move = msg % 100;
        msg /= 100;
        if(move != 0) {
            move -= 11;
            board[move / 10][move % 10] = 3 - player_number;
        }
        if((msg == 0) || (msg == 6)) {
            // Calculate minimax
            move = best_move(maximizing_player);
            board[move / 10][move % 10] = player_number;

            // Send move back to server
            player_message = std::to_string(move + 11);
            if(send(sock, player_message.c_str(), player_message.size(), 0) < 0) {
                std::cerr << "Error sending move" << std::endl;
                return -1;
            }
        } else {
            // Game over
            game_over = true;
            if(msg == 1) {
                std::cout << "You win!" << std::endl;
            } else if(msg == 2) {
                std::cout << "You lose!" << std::endl;
            } else if(msg == 3) {
                std::cout << "It's a draw!" << std::endl;
            }
        }
    }
}