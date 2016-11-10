#!/usr/bin/env python
"""Retrieve clipboard contents in a variety of formats.

Based on code from http://stackoverflow.com/questions/3261379/getting-html-source-or-rich-text-from-the-x-clipboard/3263632#3263632
"""

from argparse import ArgumentParser
import gtk
from sys import stdout, stderr, argv

from logging import Logger, StreamHandler, DEBUG, WARNING
logger = Logger('clipsource')
handler = StreamHandler(stderr)
handler.setLevel(WARNING)
logger.addHandler(handler)

def get_targets():
    """List the available targets (formats) for the current clipboard."""
    return gtk.Clipboard().wait_for_targets()

def print_targets_list(options):
    """Print the available targets (formats) for the current clipboard."""
    options.file.writelines(t + "\n" for t in get_targets())

def format_contents(contents):
    if contents.type == 'INTEGER':
        from struct import unpack
        # This would be useful for the 'TIMESTAMP' target
        #   if the integers returned were UNIX timestamps.
        # However they seem to be negative numbers.
        # As it is,
        #    I have no idea what the byte order and size can be expected to be,
        #    and haven't found documentation about how to interpret the value.
        # This mostly just prevents unprintables from being output.
        return str(unpack('i', contents.data[:4])[0])
    else:
        return contents.data

def print_target(options):
    """Output the contents of a target to a file or stream."""
    contents = gtk.Clipboard().wait_for_contents(options.target)
    if contents:
        options.file.write(format_contents(contents))

class OutputFile(file):
    """Wraps a file which should be closed at script end."""
    def __init__(self, filename, openflags='wb'):
        self.needs_close = 1
        super(OutputFile, self).__init__(filename, openflags)

if __name__ == '__main__':
    try:
        parser = ArgumentParser()
        parser.add_argument('-d', '--debug', action='store_const', const=DEBUG, default=WARNING, dest='loglevel',
            help='enable debug tracing')
        parser.add_argument('-f', '--file', action='store', default=stdout, type=OutputFile,
            help='the file to which output will be directed')
        group = parser.add_mutually_exclusive_group(required=True)
        group.add_argument(nargs='?', action='store', dest='target', choices=get_targets(),
            help='display the contents of this target', metavar="TARGET")
        group.add_argument('-l', '--list', action='store_const', const=print_targets_list, default=print_target, dest='action',
            help='list available targets')

        namespace = parser.parse_args()

        handler.setLevel(namespace.loglevel)

        logger.debug(namespace)

        namespace.action(namespace)

    finally:
        if (vars().has_key('namespace') and hasattr(namespace, 'file')
          and hasattr(namespace.file, 'needs_close')):
            logger.debug('closing file {0}'.format(namespace.file))
            namespace.file.close()
