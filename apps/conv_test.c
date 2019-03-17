#include <stdio.h>
#include <stdlib.h>

int main(int argc, char const *argv[])
{
    int i = 0, j;
    // int *in= malloc(sizeof(int)*8*8*32);
    // int *w = malloc(sizeof(int) * 5 * 5 * 32*16);
    // int *out = malloc(sizeof(int) * 4*4*16);
    int in[8][8][32] = {1};
    int w[32][16][5][5] = {1};
    int out[4][4][16] = {0};
    int row, col, ti, to;
    for (row = 0; row < 8; row++)
    {

        for (col = 0; col < 8; col++)
        {

            for (ti = 0; ti < 32; ti++)
            {
                in[row][col][ti] = 1;
            }
        }
    }
    for (ti = 0; ti < 32; ti++)
    {

        for (to = 0; to < 16; to++)
        {

            for (i = 0; i < 5; i++)
            {

                for (j = 0; j < 5; j++)
                {
                    w[ti][to][i][j] = 1;
                }
            }
        }
    }
    // for (i = 0; i < 8 * 8 * 32 * 2 / 2; ++i)
    // {
    //     if (i < 16) //0
    //         in[i] = 0x1;
    //     if (16 <= i && i < 32) //1
    //         in[i] = 0x1;
    //     else
    //         in[i] = 0x1; //0xABCD0000 + i;
    // }
    // for (j = 0; j < 5 * 5 * 32 * 16; ++j)
    // {
    //     if (j < 32) //K0_0
    //         w[j] = 0x1;
    //     else if (32 <= j && j < 64) //K1_0
    //         w[j] = 0x1;
    //     else if (64 <= j && j < 96) //K2_0
    //         w[j] = 0x1;
    //     else
    //         w[j] = 1;
    // }

    for (row = 0; row < 8; row++)
    {

        for (col = 0; col < 8; col++)
        {

            for (ti = 0; ti < 32; ti++)
            {

                for (to = 0; to < 16; to++)
                {

                    for (i = 0; i < 5; i++)
                    {

                        for (j = 0; j < 5; j++)
                        {
                            if (row + i < 8 && col + j < 8)
                                out[row][col][to] += w[ti][to][i][j] * in[row + i][col + j][ti];
                        }
                    }
                }
            }
        }
    }

    for (i = 0; i < 4; i++)
    {
        for (j = 0; j < 4; j++)
        {
            for (ti = 0; ti < 16; ti++)
            {
                printf("%d ", out[i][j][ti]);
            }
            printf("\n");
        }

        // if (i % 16 == 15)
    }

    return 0;
}