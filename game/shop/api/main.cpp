#include "crow_all.h"
#include <fstream>

int main()
{
    crow::SimpleApp app;

    // Load products from JSON
    std::ifstream file("products.json");
    std::string products((std::istreambuf_iterator<char>(file)),
                         std::istreambuf_iterator<char>());

    CROW_ROUTE(app, "/products").methods("GET"_method)([&](){
        return crow::response(products);
    });

    CROW_ROUTE(app, "/checkout").methods("POST"_method)([](const crow::request& req){
        return crow::response(200, "Checkout complete!");
    });

    app.port(8080).multithreaded().run();
}
