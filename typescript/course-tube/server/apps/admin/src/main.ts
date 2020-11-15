import { NestFactory } from '@nestjs/core';
import { SwaggerModule, DocumentBuilder } from '@nestjs/swagger';
import { AppModule } from './app.module';
import { NestExpressApplication } from '@nestjs/platform-express';

async function bootstrap() {
  const app = await NestFactory.create<NestExpressApplication>(AppModule);

  app.enableCors();
  app.useStaticAssets('uploads', {
    prefix: '/uploads',
  });

  const options = new DocumentBuilder()
    .setTitle('Course Tube API')
    .setDescription('create and share')
    .setVersion('1.0')
    .build();

  const document = SwaggerModule.createDocument(app, options);
  SwaggerModule.setup('api-docs', app, document);

  const port = process.env.ADMIN_PORT || 3002;
  await app.listen(port);
  console.log(`http://localhost:${port}/api-docs`);
}
bootstrap();
