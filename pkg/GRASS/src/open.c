/*
 *****************************************************************
 * open routines
 *
 * G__open (element, name, mapset, mode)
 *      char *element         database element name
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *      int mode              0=read, 1=write, 2=read/write
 * 
 *      this is the lowest level open routine.
 *      opens the file 'name' in 'element' ("cell", etc)
 *      in mapset 'mapset' according to the i/o 'mode'
 *
 *      mode = 0 (read) will look for 'name' in 'mapset'
 *               and open the file for read only
 *               the file must exist
 *
 *      mode = 1 (write) will create an empty file 'name' in the
 *               current mapset and open the file for write only
 *               'mapset' ignored
 *
 *      mode = 2 (read and write) will open a file in the
 *               current mapset for reading and writing
 *               creating a new file if necessary
 *               'mapset' ignored
 *
 *      returns: open file descriptor (int)
 *               or -1 could not open
 *
 *******************************************************************
 * G_open_new (element, name)
 *      char *element         database element name
 *      char *name            map file name
 *
 *      creates 'name' in the current mapset and opens it
 *      for write only.
 *
 *      returns: open file descriptor (int)
 *               or -1 could not open
 *
 *******************************************************************
 * G_open_old (element, name, mapset)
 *      char *element         database element name
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 *      opens 'name' in 'mapset' for read only.
 *
 *      returns: open file descriptor (int)
 *               or -1 could not open
 *
 *******************************************************************
 * G_fopen_new (element, name)
 *      char *element         database element name
 *      char *name            map file name
 *
 *      creates 'name' in the current mapset and opens it
 *      for write only.
 *
 *      returns: open file descriptor (FILE *)
 *               or NULL could not open
 *
 *******************************************************************
 * G_fopen_old (element, name, mapset)
 *      char *element         database element name
 *      char *name            map file name
 *
 *      opens 'name' in 'mapset' for read only.
 *
 *      returns: open file descriptor (FILE *)
 *               or NULL could not open
 *******************************************************************/

#include "config.h"
#include <string.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "gis.h"
#include <unistd.h>
#include <fcntl.h>

int G__open (
    char *element,
    char *name,
    char *mapset,
    int mode)
{
    char path[1024];
    char xname[512], xmapset[512], *dummy;
    int G_fd;


    G__check_gisinit();

/* READ */
    if (mode == 0)
    {
	if (G__name_is_fully_qualified (name, xname, xmapset))
	{
	    if (strcmp (xmapset, mapset) != 0) {
		fprintf(stderr, "G__open(r): mapset (%s) doesn't match xmapset (%s)\n",
			mapset,xmapset);
		    return -1;
	    }
	    name = xname;
	}
	if ((dummy = G_find_file (element, name, mapset)) == NULL)
	    return -1;
	G_free (dummy);
	G__file_name (path, element, name, mapset);

        G_fd = open (path, 0);
#if defined R_GRASS_INTERFACE && defined __MINGW32_VERSION
        setmode(G_fd, O_BINARY);
#endif /* __MINGW32_VERSION && R_GRASS_INTERFACE */
        return G_fd;
    }
/* WRITE */
    if (mode == 1 || mode == 2)
    {
	if (G__name_is_fully_qualified (name, xname, xmapset))
	{
	    if (strcmp (xmapset, G_mapset()) != 0) {
		fprintf(stderr, "G__open(w): xmapset (%s) != G_mapset() (%s)\n",
			xmapset,G_mapset());
		return -1;
	    }
	    name = xname;
	}

	if (G_legal_filename(name) == -1)
	    return -1;

	G__file_name (path, element, name, G_mapset());
	if(mode == 1 || access(path,0) != 0)
	{
	    G__make_mapset_element (element);
	    close (creat (path, 0666));
	}

      G_fd = open (path, mode);
#if defined R_GRASS_INTERFACE && defined __MINGW32_VERSION
      setmode(G_fd, O_BINARY);
#endif /* __MINGW32_VERSION && R_GRASS_INTERFACE */
      return G_fd;
   }
    return -1;
}

int G_open_new (char *element,char *name)
{
    return G__open (element, name, G_mapset(), 1);
}

int G_open_old (char *element,char *name,char *mapset)
{
    return G__open (element, name, mapset, 0);
}

int G_open_update (char *element,char *name)
{
    int fd;
    fd = G__open (element, name, G_mapset(), 2);
    if (fd >= 0) lseek (fd, 0L, 2);
    return fd;
}

FILE *G_fopen_new (char *element,char *name)
{
    int fd;

    fd = G__open (element, name, G_mapset(), 1);
    if (fd < 0)
	return (FILE *) 0;

#if defined R_GRASS_INTERFACE && defined __MINGW32_VERSION
    return fdopen (fd, "wb");
#else /* __MINGW32_VERSION && R_GRASS_INTERFACE */
    return fdopen (fd, "w");
#endif /* __MINGW32_VERSION && R_GRASS_INTERFACE */
}

FILE *
G_fopen_old (char *element,char *name,char *mapset)
{
    int fd;

    fd = G__open (element, name, mapset, 0);
    if (fd < 0)
	return (FILE *) 0;

#if defined R_GRASS_INTERFACE && defined __MINGW32_VERSION
    return fdopen (fd, "rb");
#else /* __MINGW32_VERSION && R_GRASS_INTERFACE */
    return fdopen (fd, "r");
#endif /* __MINGW32_VERSION && R_GRASS_INTERFACE */
}

FILE *
G_fopen_append (char *element,char *name)
{
    int fd;

    fd = G__open (element, name, G_mapset(), 2);
    if (fd < 0)
	return (FILE *) 0;
    lseek (fd, 0L, 2);

#if defined R_GRASS_INTERFACE && defined __MINGW32_VERSION
    return fdopen (fd, "ab");
#else /* __MINGW32_VERSION && R_GRASS_INTERFACE */
    return fdopen (fd, "a");
#endif /* __MINGW32_VERSION && R_GRASS_INTERFACE */
}

FILE *G_fopen_modify (char *element,char *name)
{
    int fd;

    fd = G__open (element, name, G_mapset(), 2);
    if (fd < 0)
	return (FILE *) 0;
    lseek (fd, 0L, 0);

#if defined R_GRASS_INTERFACE && defined __MINGW32_VERSION
    return fdopen (fd, "rb+");
#else /* __MINGW32_VERSION && R_GRASS_INTERFACE */
    return fdopen (fd, "r+");
#endif /* __MINGW32_VERSION && R_GRASS_INTERFACE */
}
