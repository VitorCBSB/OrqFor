CREATE TABLE IF NOT EXISTS estados_processamento (
    id        INTEGER PRIMARY KEY,
    codigo    TEXT NOT NULL UNIQUE,
    descricao TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS usuarios (
    email         TEXT PRIMARY KEY,
    nome          TEXT NOT NULL,
    matricula     TEXT NOT NULL,
    criado_em     TEXT NOT NULL,
    ultimo_acesso TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS workers (
    id               TEXT PRIMARY KEY,
    ip_address       TEXT,
    ativa            INTEGER NOT NULL DEFAULT 1,
    ultimo_heartbeat TEXT
);

CREATE TABLE IF NOT EXISTS demandas (
    id                 INTEGER PRIMARY KEY AUTOINCREMENT,
    usuario_email      TEXT NOT NULL REFERENCES usuarios(email),
    worker_id          TEXT REFERENCES workers(id),
    estado_id          INTEGER NOT NULL REFERENCES estados_processamento(id),
    diretorio_entrada  TEXT NOT NULL,
    nome_produto_final TEXT NOT NULL,
    config_json        TEXT NOT NULL,
    urgente            INTEGER NOT NULL DEFAULT 0,
    caminho_saida      TEXT,
    mensagem_erro      TEXT,
    submetida_em       TEXT NOT NULL,
    iniciada_em        TEXT,
    concluida_em       TEXT
);

INSERT OR IGNORE INTO estados_processamento (id, codigo, descricao) VALUES
    (1, 'NA_FILA',         'Na fila (aguardando máquina)'),
    (2, 'INICIALIZANDO',   'Inicializando ambiente'),
    (3, 'EXECUTANDO_PA',   'Executando PA'),
    (4, 'EXECUTANDO_IPED', 'Executando IPED'),
    (5, 'FINALIZADO',      'Concluído com sucesso'),
    (6, 'ERRO',            'Falha no processamento'),
    (7, 'CANCELADO',       'Cancelado pelo usuário');

INSERT OR IGNORE INTO workers (id, ip_address, ativa) VALUES
    ('WIN-WORKER-01', NULL, 1);