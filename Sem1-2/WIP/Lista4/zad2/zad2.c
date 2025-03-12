// zad2.c
// Nel Skowronek 279679

#include <stdbool.h>
#include <stdio.h>

bool compare(short try_ori[4], short red, short white, short code_ori[4])
{
    short try[4] = {try_ori[0], try_ori[1], try_ori[2], try_ori[3]};
    short code[4] = {code_ori[0], code_ori[1], code_ori[2], code_ori[3]};
    short local_red = 0, local_white = 0;
    for(short i = 0; i < 4; ++i)
    {
        if(try[i] == code[i])
        {
            ++local_red;
            code[i] = -1;
            try[i] = -1;
        }
    }
    if(local_red != red)
        return false;
    
    for(short i = 0; i < 4; ++i)
    {
        if(try[i] == -1)
            continue;
        for(short j = 0; j < 4; ++j)
        {
            if(code[j] == -1)
                continue;
            if(try[i] == code[j])
            {
                ++local_white;
                code[j] = -1;
                break;
            }
        }
    }
    if(local_white != white)
        return false;

    return true;
}

int main(void)
{
    bool codes[6][6][6][6];
    for(short a = 0; a < 6; ++a)
    {
        for(short b = 0; b < 6; ++b)
        {
            for(short c = 0; c < 6; ++c)
            {
                for(short d = 0; d < 6; ++d)
                {
                    codes[a][b][c][d] = true;
                }
            }
        }
    }

    int left = 6*6*6*6;
    short try[4] = {0, 0, 0, 0};

    while(left > 1)
    {
        short red, white;
        printf("%d%d%d%d\n", try[0] + 1, try[1] + 1, try[2] + 1, try[3] + 1);
        printf("Red: ");
        scanf("%hd", &red);
        if(red == 4)
            break;
        --left;
        printf("White: ");
        scanf("%hd", &white);

        bool min_found = false;
        short a = try[0], b = try[1], c = try[2], d = try[3] + 1;
        short new_try[4];
        for(;a < 6; ++a)
        {
            for(;b < 6; ++b)
            {
                for(;c < 6; ++c)
                {
                    for(;d < 6; ++d)
                    {
                        if(codes[a][b][c][d] == false)
                            continue;

                        short code[] = {a, b, c, d};
                        if(compare(try, red, white, code) == false)
                        {
                            codes[a][b][c][d] = false;
                            --left;
                        }
                        else if(min_found == false)
                        {
                            new_try[0] = a;
                            new_try[1] = b;
                            new_try[2] = c;
                            new_try[3] = d;
                            min_found = true;
                        }
                    }
                    d = 0;
                }
                c = 0;
            }
            b = 0;
        }
        for(short i = 0; i < 4; ++i)
            try[i] = new_try[i];
    }

    if(left >= 1)
        printf("Twoj kod: %d%d%d%d\n", try[0] + 1, try[1] + 1, try[2] + 1, try[3] + 1);
    else
        printf("Oszukales!\n");
}
