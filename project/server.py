import argparse
import datetime
import asyncio
import aiohttp
import json
import os
import logging
import const

class Server:
    def __init__(self, server):
        self.server = server
        self.port = const.SERVERS[server]
        self.client_dict = {}

        # create timestamped server log file
        if not os.path.exists('logs'):
            os.makedirs('logs')

        now = datetime.datetime.now()
        now_string = now.strftime('%Y-%m-%dT%H:%M:%S')
        logging.basicConfig(filename=f'logs/{server}-{now_string}.txt', encoding='utf-8', 
                            format='%(asctime)s %(levelname)-8s %(message)s', level=logging.DEBUG)
        logging.info(f'Opened server {server} session')
    
    async def handle_input(self, reader, writer):
        while not reader.at_eof():
            data = await reader.readline()
            message = data.decode()
            addr = writer.get_extra_info('peername')
            self.print_and_log(f'Received from {addr}: {message}')

            resp = await self.parse_message(message)
            if resp:
                writer.write(resp.encode())
                self.print_and_log(f"Responded to client {addr}: {resp}")

            writer.close()
            self.print_and_log(f'Closed the connection with {addr}')

    async def parse_message(self, message):
        command_table = {
            'IAMAT': self.handle_i_am_at,
            'WHATSAT': self.handle_whats_at,
            'AT': self.handle_at
        }
        num_args = {
            'IAMAT': 4,
            'WHATSAT': 4,
            'AT': 6
        }
        message_list = [msg for msg in message.strip().split() if len(msg)]
        if len(message_list) == 0:
            self.print_and_error(f'Invalid message: {message}')
            return f'? {message}'
        if not message_list[0] in command_table:
            self.print_and_error(f'Invalid message: {message}')
            return f'? {message}'
        if len(message_list) != num_args[message_list[0]]:
            self.print_and_error(f'Invalid number of args for {message_list[0]}: {len(message_list)}')
            return f'? {message}'

        cmd = command_table.get(message_list[0])
        try:
            response = await cmd(*message_list[1:])
            return response
        except Exception as e:
            self.print_and_error(f'Error while handling command: {e}')
            return f'? {message}'

    async def handle_i_am_at(self, client, coord, timestamp):
        coords = [s for s in coord.replace('-', '+').split('+') if s != ""]
        if len(coords) != 2 or not self.is_numeric(coords[0]) or not self.is_numeric(coords[1]):
            raise Exception(f'invalid IAMAT message coord field: {coord}')
        if not self.is_numeric(timestamp):
            raise Exception(f'invalid IAMAT message timestamp field: {timestamp}')

        time_diff = datetime.datetime.now().timestamp() - float(timestamp)
        time_str = '+'+str(time_diff) if time_diff > 0 else str(time_diff)
        response_str = f'AT {self.server} {time_str} {client} {coord} {timestamp}'
        
        self.client_dict[client] = {'timestamp':timestamp, 'msg':response_str}
        await self.propagate(response_str)
        return response_str

    async def handle_whats_at(self, client, radius, numplaces):
        if not client in self.client_dict:
            raise Exception(f'invalid WHATSAT message client field: {client}')
        if not self.is_numeric(radius) or float(radius) < 0 or float(radius) > 50:
            raise Exception(f'invalid WHATSAT message radius field: {radius}')
        if not self.is_numeric(numplaces) or float(numplaces) < 0 or float(numplaces) > 20:
            raise Exception(f'invalid WHATSAT message numplaces field: {numplaces}')

        at_str = self.client_dict[client]['msg']
        coord = at_str.split()[4]
        places_str = (await self.find_places(coord, radius, numplaces)).rstrip('\n')
        return f'{at_str}\n{places_str}\n\n'

    async def handle_at(self, server, time_diff, client, coord, timestamp):
        # no need to error check because only servers will send AT's
        if not client in self.client_dict or timestamp > self.client_dict[client]['timestamp']:
            at_str = f'AT {server} {time_diff} {client} {coord} {timestamp}'
            self.client_dict[client] = {'timestamp':timestamp, 'msg':at_str}
            self.print_and_log(f'Received new propagated information about location of {client}')
            await self.propagate(at_str)
        else:
            self.print_and_log(f'Received redundant propagated info for client {client}, ignoring.')
    
    async def find_places(self, coord, radius, numplaces):
        async with aiohttp.ClientSession() as session:
            url = f'https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={self.parse_coord(coord)}&radius={radius}&key={const.APIKEY}'
            self.print_and_log(f'Attempting to GET from {url}')
            async with session.get(url) as response:
                places = await response.json(loads=json.loads)

            self.print_and_log(f'Retrieved {len(places["results"])} places from Google Places API.')
            if len(places['results']) > int(numplaces):
                places['results'] = places['results'][:int(numplaces)]
                
            return str(json.dumps(places, indent=4))


    async def propagate(self, message):
        for neighbor in const.CONNECTIONS[self.server]:
            try:
                _, writer = await asyncio.open_connection(const.LOCALHOST, const.SERVERS[neighbor])
                
                writer.write(message.encode())
                self.print_and_log(f'Propagating message to {neighbor}: {message}')

                writer.close()
                await writer.wait_closed()
                self.print_and_log(f'Closing connection with {neighbor}')
            
            except:
                self.print_and_log(f'Error connecting to server {neighbor}')

    async def serve(self):
        server = await asyncio.start_server(self.handle_input, const.LOCALHOST, self.port)
        addr = server.sockets[0].getsockname()
        self.print_and_log(f'{self.server} serving on {addr}')
        async with server:
            await server.serve_forever()

    def run(self):
        try:
            asyncio.run(self.serve())
        except KeyboardInterrupt:
            self.print_and_log(f'Server {self.server} closed.')

    def parse_coord(self, coord):
        split = max(coord.rfind('+'), coord.rfind('-'))
        return f'{coord[:split]},{coord[split:]}'

    def is_numeric(self, string):
        try:
            float(string)
            return True
        except ValueError:
            return False

    def print_and_log(self, message):
        print(message)
        logging.info(message)

    def print_and_error(self, message):
        print(message)
        logging.error(message)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Proxy herd server')
    parser.add_argument('server', type=str)
    args = parser.parse_args()
    if not args.server in const.SERVERS:
        print(f'Invalid server: {args.server}')
        exit()
    s = Server(args.server)
    s.run()
    