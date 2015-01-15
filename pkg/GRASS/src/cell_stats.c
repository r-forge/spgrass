#include "gis.h"
#include <stdlib.h>

#define INCR 10
#define SHIFT 6

static int NCATS = 1<<SHIFT;

#define NODE struct Cell_stats_node

static int next_node (struct Cell_stats *);
static int init_node(NODE *,int,int);

int G_init_cell_stats(struct Cell_stats *s)
{
    s->N = 0;
    s->tlen = INCR;
    s->node = (NODE *) G_malloc (s->tlen * sizeof (NODE));
    s->null_data_count = 0;

    return 1;
}

int G_update_cell_stats (CELL *cell, int n, struct Cell_stats *s)
{
    CELL cat;
    register int p,q;
    int idx, offset;
    int N;
    register NODE *node, *pnode;
    register NODE *new_node;

    if (n <= 0) return 1;

    node = s->node;

/* first non-null node is special case */
    if ((N = s->N) == 0)
    {
	cat = *cell++;
        while(G_is_c_null_value(&cat))
	{
            s->null_data_count++;
	    cat = *cell++;
	    n--;
        }
	if(n>0) /* if there are some non-null cells */
	{
	   N = 1;
	   if (cat < 0)
	   {
	       idx = -((-cat) >> SHIFT) - 1;
	       offset = cat + ((-idx) << SHIFT) - 1;
	   }
	   else
	   {
	       idx = cat >> SHIFT;
	       offset = cat - (idx << SHIFT);
	   }
	   fflush(stderr);
	   init_node (&node[1], idx, offset);
	   node[1].right = 0;
	   n--;
        }
    }
    while (n-- > 0)
    {
	cat = *cell++;
        if(G_is_c_null_value(&cat))
        {
            s->null_data_count++;
            continue;
        }
	if (cat < 0)
	{
	    idx = -((-cat) >> SHIFT) - 1;
	    offset = cat + ((-idx) << SHIFT) - 1;
	}
	else
	{
	    idx = cat >> SHIFT;
	    offset = cat - (idx << SHIFT);
	}

	q = 1;
	while (q > 0)
	{
	    pnode = &node[p = q];
	    if (pnode->idx == idx)
	    {
		pnode->count[offset]++;
		break;
	    }
	    if (pnode->idx > idx)
		q = pnode->left;             /* go left */
	    else
		q = pnode->right;            /* go right */
	}
	if (q > 0)
	    continue;				/* found */

    /* new node */
	N++;

    /* grow the tree? */
	if (N >= s->tlen)
	{
	    node = (NODE *) G_realloc ((char *) node, sizeof(NODE) * (s->tlen += INCR));
	    pnode = &node[p]; /* realloc moves node, must reassign pnode */
	}

    /* add node to tree */
	init_node (new_node = &node[N], idx, offset);

	if (pnode->idx > idx)
	{
	    new_node->right = -p;            /* create thread */
	    pnode->left  = N;                /* insert left */
	}
	else
	{
	    new_node->right = pnode->right; /* copy right link/thread */
	    pnode->right = N;                /* add right */
	}
    } /* while n-- > 0 */
    s->N = N;
    s->node = node;

    return 0;
}

static int init_node(NODE *node,int idx,int offset)
{
    register long *count;
    register int i;

    count = node->count = (long *) G_calloc (i = NCATS, sizeof(long));
    while(i--)
	*count++ = 0;
    node->idx = idx;
    node->count[offset] = 1;
    node->left = 0;

    return 0;
}

int G_find_cell_stat (
    CELL cat,
    long *count,
    struct Cell_stats *s)
{
    register int q;
    register int idx;
    int offset;

    *count = 0;
    if(G_is_c_null_value(&cat))	
    {
       *count = s->null_data_count;
       return (*count != 0);
    }

    if (s->N <= 0)
	return 0;

/*
    if (cat < 0)
    {
	idx = -(-cat/NCATS) - 1;
	offset = cat - idx*NCATS - 1;
    }
    else
    {
	idx = cat/NCATS;
	offset = cat - idx*NCATS;
    }
*/
    if (cat < 0)
    {
	idx = -((-cat) >> SHIFT) - 1;
	offset = cat + ((-idx) << SHIFT) - 1;
    }
    else
    {
	idx = cat >> SHIFT;
	offset = cat - (idx << SHIFT);
    }

    q = 1;
    while (q > 0)
    {
	if (s->node[q].idx == idx)
	{
	    *count = s->node[q].count[offset];
	    return (*count != 0);
	}
	if (s->node[q].idx > idx)
	    q = s->node[q].left;             /* go left */
	else
	    q = s->node[q].right;            /* go right */
    }
    return 0;
}

int G_rewind_cell_stats (struct Cell_stats *s)
{
    int q;

    if (s->N <= 0)
	return 1;
/* start at root and go all the way to the left */
    s->curp = 1;
    while ((q = (s->node[s->curp].left)))
	s->curp = q;
    s->curoffset = -1;

    return 0;
}

static int next_node (struct Cell_stats *s)
{
    int q;

/* go to the right */
    s->curp = s->node[s->curp].right;

    if (s->curp == 0)          /* no more */
	return 0;

    if (s->curp < 0)           /* thread. stop here */
    {
	s->curp = -(s->curp) ;
	return 1;
    }

    while ((q = (s->node[s->curp].left)))   /* now go all the way left */
	s->curp = q;

    return 1;
}

int G_next_cell_stat (
    CELL *cat,
    long *count,
    struct Cell_stats *s)
{
    int idx;

    /* first time report stats for null */
    /* decided not to return stats for null in this function 
    static int null_reported = 0;
    if(!null_reported && s->null_data_count > 0)
    {
       *count = s->null_data_count;
       G_set_c_null_value(&cat,1);
       null_reported = 1;
       return 1;
    }
    */
    if (s->N <= 0)
	return 0;
    for(;;)
    {
	s->curoffset++;
	if (s->curoffset >= NCATS)
	{
	    if(!next_node(s))
		return 0;
	    s->curoffset = -1;
	    continue;
	}
	if ((*count = (s->node[s->curp].count[s->curoffset])))
	{
	    idx = s->node[s->curp].idx;

	/*
	    if (idx < 0)
		*cat = idx*NCATS + s->curoffset+1;
	    else
		*cat = idx*NCATS + s->curoffset;
	*/
	    if (idx < 0)
		*cat = -((-idx)<<SHIFT) + s->curoffset+1;
	    else
		*cat = (idx<<SHIFT) + s->curoffset;

	    return 1;
	}
    }
}

int G_get_stats_for_null_value (long *count, struct Cell_stats *s)
{
    *count =  s->null_data_count;
    return 1;
}

int G_free_cell_stats (struct Cell_stats *s)
{
    int i;

    for (i = 1; i <= s->N; i++)
	free (s->node[i].count);
    free (s->node);

    return 0;
}
