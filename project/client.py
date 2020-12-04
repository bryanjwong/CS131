import argparse
import datetime
import asyncio
import os
import logging
import const

class Client:
    def __init__(self, server):
        self.server = server
        self.port = const.SERVERS[server]

        # create timestamped client log file
        if not os.path.exists('logs):
            os.makedirs('logs')

        now = datetime.datetime.now()
        now_string = now.strftime('%Y-%m-%dT%H:%M:%S')
        logging.basicConfig(filename=f'logs/client-{now_string}.txt', encoding='utf-8', 
                            format='%(asctime)s %(levelname)-8s %(message)s', level=logging.DEBUG)
        logging.info('Opened client session')

    async def tcp_echo_client(self, message):
        try:
            reader, writer = await asyncio.open_connection(const.LOCALHOST, self.port)
        except OSError:
            self.print_and_error(f'Failed to connect to server: {self.server}')
            exit()

        writer.write((message + '\n').encode())
        self.print_and_log(f'Sent to {self.server}: {message}')

        data = await reader.read()
        self.print_and_log(f'Received from {self.server}: {data.decode()}')

        writer.close()
        self.print_and_log(f'Closed the connection with {self.server}\n')

    def read_input(self):
        while True:
            try:
                msg = input('Input your message: ')
                asyncio.run(self.tcp_echo_client(msg))
            except KeyboardInterrupt:
                self.print_and_log('Session ended.')
                break

    def print_and_log(self, message):
        print(message)
        logging.info(message)

    def print_and_error(self, message):
        print(message)
        logging.error(message)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Proxy herd client')
    parser.add_argument('server', type=str)
    args = parser.parse_args()
    if not args.server in const.SERVERS:
        print(f'Invalid server: {args.server}')
        exit()
    c = Client(args.server)
    c.read_input()