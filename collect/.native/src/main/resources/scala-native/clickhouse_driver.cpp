#include <clickhouse/client.h>
#include <stdio.h>
#include <iostream>

using namespace clickhouse;

extern "C" Client *getClient() {
    return new Client(ClientOptions().SetHost("localhost"));
}

extern "C" void execute(Client *client, const char *query) {
    client->Execute(query);
}

extern "C" void query(Client *client, const char *query, void * context, bool (*callback)(const Block *, void *)) {
    client->SelectCancelable(query, [&](const Block &block) {
        return callback(&block, context);
    });
}

extern "C" long long block_getRowCount(Block *block) {
    return block->GetRowCount();
}

extern "C" long long block_getColumnCount(Block *block) {
    return block->GetColumnCount();
}

extern "C" Column *block_getColumn(Block *block, long long index) {
    return (*block)[index].get();
}

extern "C" short column_type(Column *column) {
    return column->Type()->GetCode();
}

extern "C" void *column_at(Column *column, long long index) {
    switch (column->Type()->GetCode()) {
        case Type::Int8:
            return (void *) &(*column->As<ColumnInt8>())[index];
        case Type::Int16:
            return (void *) &(*column->As<ColumnInt16>())[index];
        case Type::Int32:
            return (void *) &(*column->As<ColumnInt32>())[index];
        case Type::Int64:
            return (void *) &(*column->As<ColumnInt64>())[index];
        case Type::UInt8:
            return (void *) &(*column->As<ColumnUInt8>())[index];
        case Type::UInt16:
            return (void *) &(*column->As<ColumnUInt16>())[index];
        case Type::UInt32:
            return (void *) &(*column->As<ColumnUInt32>())[index];
        case Type::UInt64:
            return (void *) &(*column->As<ColumnUInt64>())[index];
        case Type::Float32:
            return (void *) &(*column->As<ColumnFloat32>())[index];
        case Type::Float64:
            return (void *) &(*column->As<ColumnFloat64>())[index];
        case Type::String:
            return (void *) &(*column->As<ColumnString>())[index];
        case Type::DateTime:
            return (void *) (*column->As<ColumnDateTime>()).At(index); // !!!!!!!! do not take pointer
        case Type::Date:
            return (void *) (*column->As<ColumnDate>()).At(index); // !!!!!!!! do not take pointer
        case Type::Nullable:
            return nullptr;
//            return (void *) &(*column->As<ColumnNullable>())[index];
        default:
            return nullptr;
    }
}