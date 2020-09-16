#!/usr/bin/env
import json
import os, sys
from struct import unpack, pack
import logging

# logging.basicConfig(filename='detect.log',level=logging.DEBUG)
logger = logging.getLogger(__name__)

# create logger
# logger = logging.getLogger('simple_example')
logger.setLevel(logging.DEBUG)

# create console handler and set level to debug
ch = logging.FileHandler(filename='detect.log')
# ch.setLevel(logging.DEBUG)

# create formatter
formatter = logging.Formatter(
    '[%(levelname)1.1s %(asctime)s %(module)s:%(lineno)d] %(message)s')

# add formatter to ch
ch.setFormatter(formatter)

# add ch to logger
logger.addHandler(ch)


UUID4_SIZE = 16

# setup of FD 3 for input (instead of stdin)
# FD 4 for output (instead of stdout)
def setup_io():
    logger.debug('setup_io')
    return os.fdopen(3,"rb"), os.fdopen(4,"wb")


def read_message(input_f):
    # reading the first 4 bytes with the length of the data
    # the other 32 bytes are the UUID string,
    # the rest is the image
    logger.debug('read_message: %s', input_f)

    header = input_f.read(4)
    if len(header) != 4:
        return None # EOF

    (total_msg_size, ) = unpack("!I", header)
    logger.debug('msg size: %s', total_msg_size)

    # image id
    image_id = input_f.read(UUID4_SIZE)

    # read image data
    image = input_f.read(total_msg_size - UUID4_SIZE)

    return {'id': image_id, 'image': image}


def write_result(output, msg):
    """
    Write back the same 'msg' that we received
    """
    logger.debug('write_result: %s %s', output, msg)

    result = json.dumps(msg).encode("ascii")

    header = pack("!I", len(result) + UUID4_SIZE)
    output.write(header)
    output.write(msg['id'])
    output.write(result)
    output.flush()


def main():
    logger.debug('main')
    input_f, output_f = setup_io()

    while True:
        msg = read_message(input_f)
        if msg is None:
            break

        #send result back to elixir
        write_result(output_f, msg)


if __name__ == "__main__":
    main()
