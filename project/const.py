# server/port mappings
SERVERS = {'Hill':12305, 'Jaquez':12306, 'Smith':12307, 'Campbell':12308, 'Singleton':12309}
CONNECTIONS = {
    'Hill':['Jaquez', 'Smith'], 
    'Jaquez':['Hill', 'Singleton'], 
    'Smith':['Hill', 'Singleton', 'Campbell'],
    'Campbell':['Smith', 'Singleton'],
    'Singleton':['Jaquez', 'Smith', 'Campbell']}

# default ip address
LOCALHOST = '127.0.0.1'

# api key
APIKEY = 'INSERTKEYHERE'