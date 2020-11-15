import { Module, Global } from '@nestjs/common';
import { DbService } from './db.service';
import { TypegooseModule } from 'nestjs-typegoose';
import { User } from './models/user.model';
import { Course } from './models/course.model';
import { Episode } from './models/episode.model';

const models = TypegooseModule.forFeature([User, Course, Episode]);

@Global()
@Module({
  imports: [
    TypegooseModule.forRootAsync({
      useFactory() {
        return {
          uri: process.env.DB,
          useNewUrlParser: true,
          useFindAndModify: false,
          useUnifiedTopology: true,
          useCreateIndex: true,
        };
      },
    }),
    // TypegooseModule.forRoot(process.env.DB, {
    //   useNewUrlParser: true,
    //   useFindAndModify: false,
    //   useUnifiedTopology: true,
    //   useCreateIndex: true,
    // }),
    models,
  ],
  providers: [DbService],
  exports: [DbService, models],
})
export class DbModule {}
