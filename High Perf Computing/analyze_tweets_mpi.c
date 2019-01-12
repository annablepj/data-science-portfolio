/**
*
* E517 Final Project - High Performance Twitter Analysis
* MPI Version
* Peter Annable, April 27, 2018
*
**/
#include <stdio.h>
#include <stddef.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <time.h>
#include <mpi.h>

#define PI 3.1415926536
#define BUFFSIZE 2048
#define TWEETSIZE 280
#define PHRASESIZE 40
#define DEBUG 0
#define NEGATIVE -1 
#define POSITIVE 1
#define ROOT 0
#define NEG_WORDS_FILE "./negative-words.txt"
#define POS_WORDS_FILE "./positive-words.txt"

//
// define getcols routine in tinycsv.c
//
int getcols( const char * const line, const char * const delim, char ***out_storage );

// tweets will be read into an array of tweets structs, and compared with phrases for
// positive or negative sentiment.

typedef struct {
    char   id[40];
    char   date[80];
    char   location[80];
    int    retweets;
    char   hashtags[80];
    char   text[TWEETSIZE];
    char   keywords[TWEETSIZE];
    double pos_score;
    double neg_score;
} tweet;
    
typedef struct{
    char   text[PHRASESIZE];
    double score;
} phrase; 
    
phrase* alloc_phrases_array(int ROWS){
    if (DEBUG) printf("alloc_phrases_array: calloc %d elements\n",ROWS);
    phrase* matrix;
    matrix = (phrase*) calloc(ROWS, sizeof(phrase));
    return matrix;
}

tweet* alloc_tweets_array(int ROWS){
    if (DEBUG) printf("alloc_tweets_array: calloc %d elements\n",ROWS);
    tweet* matrix;
    matrix = (tweet*) calloc(ROWS, sizeof(tweet));
    return matrix;
}

void print_tweets(tweet* t, int ROWS) {
    for (int i = 0; i < ROWS; i++) {
        printf("Id=%s, Date=[%s], Location=[%s], Retweets=%d, Hastags=[%s], Text=[%s], pos_score=%3.1f, \
        neg_score=%3.1f, keywords=[%s]\n", t[i].id, t[i].date, t[i].location, t[i].retweets, t[i].hashtags, \
        t[i].text, t[i].pos_score, t[i].neg_score, t[i].keywords);
    }
}
// 
// write out csv formatted file with tweet scoring and keywords found
//
void write_output(char* filebase, tweet* t, int ROWS) {
    FILE *file;
    char fname[80];
    time_t now=time(NULL);
    struct tm tm = *localtime(&now);
    sprintf(fname,"scores-%s-%d%02d%02d-%d:%d:%d.csv",filebase, tm.tm_year+1900, tm.tm_mon+1, tm.tm_mday, \
                                                  tm.tm_hour, tm.tm_min, tm.tm_sec);
    printf("ROOT: Writing scoring output to file [%s]\n", fname);
    file = fopen(fname, "w");
    if (file == NULL) {
       fprintf(stderr, "Error, could not open file=[%s]\n", fname);
       MPI_Finalize();
       exit(1);
     }
    fprintf(file,"id,date,pos_score,neg_score,location,keywords\n");
    for (int i = 0; i < ROWS; i++) {
      fprintf(file,"%s,%s,%3.1f,%3.1f,%s,%s\n",t[i].id, t[i].date, t[i].pos_score, t[i].neg_score, t[i].location, t[i].keywords);
    }
    fclose(file);
}

void print_phrases(phrase* phrases, int ROWS) {
    printf("\n\n\nPHRASES LISTING: %d items\n", ROWS);
    for (int i = 0; i < ROWS; i++) {
       printf("%s, %f\n", phrases[i].text, phrases[i].score);
    }
}

//
// To simplify processing, we need to know the number of lines in advance for a file
// this will allow us ot calloc tweet and phrase structures in one call to calloc.
//
int count_file_lines(char* filename) {
    int count=0;
    char line[BUFFSIZE];
    FILE *file = NULL;
    file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr,"ERROR Input file [%s] not found\n", filename); 
        exit(1);
        }
    while(fgets(line, BUFFSIZE, file) != NULL) {
        count++;
        }
    fclose(file);
    if (DEBUG) printf("count_file_lines: num lines = %d\n",count);
    return(count);
}
//
// read tweets from csv file into array of tweet structs
//
tweet* read_tweets(char* filename, int nlines) {
    FILE *file = NULL;
    const char sep[2] = ",";
    int i = 0, j=0;
    int copylen=0;
    char line[BUFFSIZE];
    tweet *tweetfile;

    tweetfile = alloc_tweets_array(nlines);

    file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr,"ERROR Input file [%s] could not be reopened\n", filename); 
        exit(1);
        }
    if (DEBUG) printf("read_tweets: Starting Loop, %d lines indicated in file\n", nlines);
    for (i=0; i<nlines; i++) {
        //
       fgets(line, BUFFSIZE, file);
       if (DEBUG) printf("read line [%s]\n", line );
       char **columnX = NULL;
       getcols(line, sep, &columnX);
       memcpy(tweetfile[i].id,columnX[0], strlen(columnX[0]));
       memcpy(tweetfile[i].date,columnX[1], strlen(columnX[1]));
       memcpy(tweetfile[i].location,columnX[2], strlen(columnX[2]));
       tweetfile[i].retweets=atoi(columnX[3]);
       memcpy(tweetfile[i].hashtags,columnX[4], strlen(columnX[4]));
       copylen=strlen(columnX[5]);
       if (copylen > TWEETSIZE) 
          copylen = TWEETSIZE;
       else
          copylen--;    // remove trailing newline
       memcpy(tweetfile[i].text,columnX[5],copylen);
       if (DEBUG) for ( j = 0; j<6; j++) printf("  Col[ %d ] = %s\n", j, columnX[j] );
       free(columnX);
       tweetfile[i].pos_score=0.0;
       tweetfile[i].neg_score=0.0;
    }
    if (DEBUG) printf("read_tweets: %d tweets read in \n", i);
    fclose(file);
    return(tweetfile);
}

//
// read a list of phrase words in to array of phrases.
// default the score value of each to 1.0
//
phrase* read_phrases(char* filename, int plines) {
    FILE *file = NULL;
    int i = 0;
    int id = 0;
    int copylen=0;
    char line[256];
    phrase *phrasefile;

    phrasefile = alloc_phrases_array(plines);

    file = fopen(filename, "r");
    if (file == NULL) {
        fprintf(stderr,"ERROR Input file [%s] could not be reopened\n", filename); 
        exit(1);
        }
    for (i=0; i<plines; i++) {
        fgets(line, 256, file);
        line[strlen(line)-1]=0;  // remove trailing newline
        strncpy(phrasefile[i].text, line, 40);
        phrasefile[i].score=1.0;
    } 
    fclose(file);
    if (DEBUG) printf("\n\nDone Reading %d lines in Phrasefile %s\n", plines, filename);
    return(phrasefile);
}

double score_tweet(tweet* t, phrase* p, int plines, int mode) {
   int i, j;
   const char sep[2] = " ";
   char *token, *saveptr;
   double score=0;
   char templine[TWEETSIZE];
   //
   // process single-word phrases
   // potential future enahancement, replace with better search routine, and support
   // multiple word phrases
   //
   strcpy(templine, t->text);
   if (DEBUG) printf("Scoring tweet text [%s] with %d phrases\n", templine, plines);
   token = strtok_r(templine, sep, &saveptr);
   while (token != NULL) {
     if (strlen(token) > 1) {    // skip single char words
        for (i=0; i<plines; i++) {
           //if (DEBUG) printf("Comparing token=[%s] with word=[%s]\n", token, p[i].text);
           if (strcmp(token, p[i].text) == 0) {
              // found a match
              if (mode == NEGATIVE) 
                score-=p[i].score;
              else
                score+=p[i].score;
              strcat(t->keywords,p[i].text); // add to list of keywords 
              strcat(t->keywords,";");   // add separator
              if (DEBUG) printf("\nscore_tweet: HIT, adding [%s] to list of keywords\n",p[i].text);
              break;  // found match so break
           }
        }
     }
     // get next token
     token = strtok_r(NULL, sep, &saveptr);
   }
   if (DEBUG) printf("sentiment score for: [%s] was [%f]\n", t->text, score);
   return(score);
}


int main(int argc, char *argv[]) {

   int negcount=0, poscount=0, nlines=0;
   int nprocs, rank;
   tweet* tweets;
   tweet* all_tweets;
   phrase* pos_phrases;
   phrase* neg_phrases;
   double sentiment;
   char infile[80];
  
   MPI_Init(&argc, &argv);
   MPI_Comm_size(MPI_COMM_WORLD, &nprocs);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   if (argc != 2) {
     fprintf(stderr, "Usage: %s input_file\n", argv[0]);
     MPI_Finalize();  
     exit(1);
   }
   strcpy(infile,argv[1]);
   // Get number of lines of supplied tweets input file
   nlines=count_file_lines(infile);
   if (nlines < nprocs ) {
      fprintf(stderr, "Error, fewer tweet lines than processes.\n");
      MPI_Finalize();
      exit(1);
   }
   //
   if (rank == ROOT) printf("ROOT: Starting sentiment analysis on file [%s]\n",infile);
   //
   // Read in positive and negative words phrases
   //
   poscount=count_file_lines(POS_WORDS_FILE);
   pos_phrases=read_phrases(POS_WORDS_FILE, poscount);
   negcount=count_file_lines(NEG_WORDS_FILE);
   neg_phrases=read_phrases(NEG_WORDS_FILE, negcount);

   if (DEBUG) {
        printf("\nMAIN: All Phrases Read in:\n\n");
   	print_phrases(pos_phrases, poscount);
   	print_phrases(neg_phrases, negcount);
   }
   

   //
   // Define MPI structure to match the tweet struct
   //
   int nitems = 9;
   MPI_Datatype types[nitems];
   MPI_Datatype mpi_tweet;
   MPI_Aint offsets[nitems]; // array of offsets
   int blocklengths[nitems];

   types[0] = MPI_CHAR;   offsets[0] = offsetof(tweet,id);blocklengths[0] = 40;
   types[1] = MPI_CHAR;   offsets[1] = offsetof(tweet,date);blocklengths[1] = 80;
   types[2] = MPI_CHAR;   offsets[2] = offsetof(tweet,location);blocklengths[2] = 80;
   types[3] = MPI_INT;    offsets[3] = offsetof(tweet,retweets);blocklengths[3] = 1;
   types[4] = MPI_CHAR;   offsets[4] = offsetof(tweet,hashtags);blocklengths[4] = 80;
   types[5] = MPI_CHAR;   offsets[5] = offsetof(tweet,text);blocklengths[5] = TWEETSIZE;
   types[6] = MPI_CHAR;   offsets[6] = offsetof(tweet,keywords);blocklengths[6] = TWEETSIZE;
   types[7] = MPI_DOUBLE; offsets[7] = offsetof(tweet,pos_score);blocklengths[7] = 1;
   types[8] = MPI_DOUBLE; offsets[8] = offsetof(tweet,neg_score);blocklengths[8] = 1;

   MPI_Type_create_struct(nitems,blocklengths,offsets,types,&mpi_tweet);
   MPI_Type_commit(&mpi_tweet);

   //
   // Root will read in tweets and scatter to all
   //
   if (rank == ROOT) {
      all_tweets=read_tweets(infile, nlines);
      if (DEBUG) printf("ROOT MAIN: Tweets Read in:\n\n");
   }
   if (rank == ROOT) printf("ROOT: %d Tweets read in, broadcasting to %d procs\n", nlines, nprocs);
   //
   // Integer divide total lines by number or proces to get number of tweets to broadcast to each
   // alloc array of tweets to receive the portion of tweets from scatter
   //
   int lines_per_proc = nlines/nprocs;
   int remainder = nlines % nprocs;

   tweets=alloc_tweets_array(lines_per_proc); 

   MPI_Scatter(all_tweets,lines_per_proc,mpi_tweet,tweets,lines_per_proc, mpi_tweet, ROOT ,MPI_COMM_WORLD);

   if (DEBUG) print_tweets(tweets, lines_per_proc);

   for (int i=0; i < lines_per_proc; i++) {
     tweets[i].pos_score=score_tweet(&tweets[i],pos_phrases, poscount, POSITIVE);
     tweets[i].neg_score=score_tweet(&tweets[i],neg_phrases, negcount, NEGATIVE);
   }
   //
   // Gather all results, blocking until each proc is done
   //
   MPI_Gather(tweets,lines_per_proc,mpi_tweet,all_tweets,lines_per_proc, mpi_tweet, ROOT, MPI_COMM_WORLD);

   if (DEBUG) {
      printf("RANK %d Output:\n", rank);
      print_tweets(tweets, lines_per_proc);
   }

   // Use ROOT rank to process the remainder of tweets left in case number of procs 
   // did not evenly divide.
   if (rank == ROOT) {
      if (remainder > 0) {
          printf("ROOT: Processing remainder of %d tweets\n", remainder);
          for (int i=nprocs*lines_per_proc; i < nlines; i++) {
            all_tweets[i].pos_score=score_tweet(&all_tweets[i],pos_phrases, poscount, POSITIVE);
           all_tweets[i].neg_score=score_tweet(&all_tweets[i],neg_phrases, negcount, NEGATIVE);
          }
      }
      //
      // root now has all results, write out to CSV formatted file
      //
      char* filebase=strtok(infile,".csv");      
      write_output(filebase, all_tweets, nlines); 
   }  
   if (rank == ROOT) printf("ROOT: Analysis Complete.\n", rank);
   MPI_Finalize();  
   return(0);
}
